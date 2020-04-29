/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include <libisp.hh>
#include "main.hh"

using namespace lisp;

#define PROMPTLENGTH 80

extern void pputc(int, FILE*);

char current_prompt[PROMPTLENGTH];
LISPT history;                 /* Holds the history list. */
LISPT histnum;                 /* Current event number. */
LISPT histmax;                 /* Maximum number of events to save. */
LISPT input_exp;               /* The input expression. */
LISPT topexp;                  /* Transformed expression to evaluate. */
LISPT alias_expanded;          /* For checking alias loops. */
LISPT (*transformhook)(LISPT); /* Applied on input if non-nullptr. */
void (*beforeprompt)();        /* Called before the prompt is printed. */

static bool printit = false; /* If the result will be printed. */

/*
 * History functions.
 */
/*
 * Print the history list.
 */
static void phist()
{
  LISPT hl;

  for(hl = history; !is_NIL(hl); hl = hl->cdr())
  {
    printf("%d.\t", hl->car()->car()->intval());
    prinbody(hl->car()->cdr(), stdout, 1);
    pputc('\n', primout);
  }
}

/*
 * Add event to history list.
 */
static void addhist(LISPT what)
{
  history = cons(cons(histnum, what), history);
  histnum = add1(histnum);
}

/*
 * Remove last event from history list.
 */
static void remhist()
{
  history = history->cdr();
  histnum = sub1(histnum);
}

/*
 * Trim history list to keep it shorter than histmax.
 */
static void trimhist()
{
  LISPT hl;
  int i;

  hl = history;
  for(i = 0; i < histmax->intval() && !is_NIL(hl); i++, hl = hl->cdr())
    ;
  if(!is_NIL(hl))
    rplacd(hl, C_NIL);
}

/*
 * Return the NUM entry from history list HLIST, or nil if there is
 * no entry.
 */
LISPT histget(int num, LISPT hlist)
{
  if(num < 0)
  {
    for(; type_of(hlist) == CONS && num < 0; hlist = hlist->cdr(), num++)
      ;
    if(is_NIL(hlist))
      return C_NIL;
    else
      return hlist->car()->cdr();
  }
  else if(num > 0)
  {
    for(; type_of(hlist) == CONS && num != hlist->car()->car()->intval(); hlist = hlist->cdr())
      ;
    if(is_NIL(hlist))
      return C_NIL;
    else
      return hlist->car()->cdr();
  }
  else if(!is_NIL(hlist))
    return hlist->car()->cdr();
  return C_NIL;
}

PRIMITIVE printhist()
{
  remhist(); /* Removes itself from history. */
  phist();
  return C_NIL;
}

static LISPT transform(LISPT list)
{
  if(transformhook != nullptr)
    return (*transformhook)(list);
  else
    return list;
}

/*
 * Expands aliases in expression EXP. If car of EXP is a literal atom 
 * findalias checks for an alias substitution on property ALIAS. If
 * it is non-nil another expansion is tried until the alias property
 * is nil. Alias looping is detected by saving each expanded atom
 * on the list alias_expanded. One indirection is allowed in order
 * to permit 'alias ls ls -F'.
 */
LISPT findalias(LISPT exp)
{
  LISPT alias;
  LISPT rval;

  rval = exp;
  while(1)
  {
    if(type_of(rval) == CONS && type_of(rval->car()) == SYMBOL)
    {
      alias = getprop(rval->car(), C_ALIAS);
      if(!is_NIL(alias) && (is_NIL(alias_expanded) || !EQ(rval->car(), alias_expanded->car())))
      {
        if(!is_NIL(memb(rval->car(), alias_expanded)))
          throw lisp_error("Alias loop");
        alias_expanded = cons(rval->car(), alias_expanded);
        rval = append(cons(alias, cons(rval->cdr(), C_NIL)));
      }
      else
        break;
    }
    else
      break;
  }
  return transform(rval);
}

void promptprint(LISPT prompt)
{
  int i;
  const char* s;
  char buf[80];

  current_prompt[0] = '\0';
  if(type_of(prompt) != STRING)
    return;
  else
  {
    s = prompt->getstr();
    for(i = 0; s[i]; i++)
    {
      if(s[i] == '!')
      {
        sprintf(buf, "%d", histnum->intval());
        strcat(current_prompt, buf);
        continue;
      }
      else if(s[i] == '\\')
        i++;
      buf[0] = s[i];
      buf[1] = '\0';
      strcat(current_prompt, buf);
    }
  }
  printf("\r");
  printf("%s", current_prompt);
}

bool toploop(LISPT* tprompt, int (*macrofun)(LISPT*))
{
  while(1)
  {
    brkflg = false;
    interrupt = false;
    printit = false;
    echoline = false;
    if(beforeprompt != nullptr)
      (*beforeprompt)();
    /*
       * Evaluate promptform and print prompt.
       */
    if(options.interactive)
    {
      if(type_of(eval(promptform)) == ERROR)
      {
        xprint(mkstring("Error in promptform, reset to nil"), C_T);
        promptform = C_NIL;
      }
      promptprint(*tprompt);
    }
    input_exp = xreadline(C_T);
    if(macrofun)
      switch((*macrofun)(&input_exp))
      {
        case 0:
          return true;
        case 1:
          break;
        case 2:
          continue;
      }
    if(type_of(input_exp) == ENDOFFILE)
      return true;
    if(EQ(input_exp->car(), C_NIL))
      continue;
    addhist(input_exp);
    if(echoline)
    {
      prinbody(input_exp, stdout, 1);
      pputc('\n', primout);
    }
    topexp = transform(input_exp);
    if(type_of(topexp->car()) == CONS)
    {
      topexp = topexp->car();
      printit = true;
    }
    alias_expanded = C_NIL;
    topexp = eval(topexp);
    if(printit)
      xprint(topexp, C_T);
    if(!options.interactive && options.command)
      return false;
    trimhist();
  }
}

void init_hist()
{
  alloc::add_mark_object(&history);
  alloc::add_mark_object(&histnum);
  alloc::add_mark_object(&histmax);
  alloc::add_mark_object(&alias_expanded);
  initcvar(&history, "history", C_NIL);
  initcvar(&histnum, "histnum", mknumber(1L));
  initcvar(&histmax, "histmax", mknumber(100L));
  mkprim(PN_PRINTHIST, printhist, 0, FSUBR);
}
