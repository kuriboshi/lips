/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 * $Id$
 */
#include <lisp.hh>
#include "main.hh"

#define PROMPTLENGTH 80

extern void pputc(int, FILE*);

char current_prompt[PROMPTLENGTH];
LISPT history;                 /* Holds the history list. */
LISPT histnum;                 /* Current event number. */
LISPT histmax;                 /* Maximum number of events to save. */
LISPT input_exp;               /* The input expression. */
LISPT topexp;                  /* Transformed expression to evaluate. */
LISPT alias_expanded;          /* For checking alias loops. */
LISPT (*transformhook)(LISPT); /* Applied on input if non-NULL. */
void (*beforeprompt)(void);    /* Called before the prompt is printed. */

static int printit; /* If the result will be printed. */

/*
 * History functions.
 */
/*
 * Print the history list.
 */
static void phist()
{
  LISPT hl;

  for (hl = history; !ISNIL(hl); hl = CDR(hl))
    {
      printf("%d.\t", INTVAL(CAR(CAR(hl))));
      prinbody(CDR(CAR(hl)), stdout, 1);
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
  history = CDR(history);
  histnum = sub1(histnum);
}

/*
 * Trim history list to keep it shorter than histmax.
 */
static void trimhist()
{
  LISPT hl;
  long i;

  hl = history;
  for (i = 0; i < INTVAL(histmax) && !ISNIL(hl); i++, hl = CDR(hl))
    ;
  if (!ISNIL(hl))
    rplacd(hl, C_NIL);
}

/*
 * Return the NUM entry from history list HLIST, or nil if there is
 * no entry.
 */
LISPT histget(long num, LISPT hlist)
{
  if (num < 0)
    {
      for (; TYPEOF(hlist) == CONS && num < 0; hlist = CDR(hlist), num++)
        ;
      if (ISNIL(hlist))
        return C_NIL;
      else
        return CDR(CAR(hlist));
    }
  else if (num > 0)
    {
      for (; TYPEOF(hlist) == CONS && num != INTVAL(CAR(CAR(hlist)));
           hlist = CDR(hlist))
        ;
      if (ISNIL(hlist))
        return C_NIL;
      else
        return CDR(CAR(hlist));
    }
  else if (!ISNIL(hlist))
    return CDR(CAR(hlist));
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
  if (transformhook != NULL)
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
  while (1)
    {
      if (TYPEOF(rval) == CONS && TYPEOF(CAR(rval)) == SYMBOL)
        {
          alias = getprop(CAR(rval), C_ALIAS);
          if (!ISNIL(alias)
            && (ISNIL(alias_expanded) || !EQ(CAR(rval), CAR(alias_expanded))))
            {
              if (!ISNIL(memb(CAR(rval), alias_expanded)))
                return error(ALIAS_LOOP, C_NIL);
              alias_expanded = cons(CAR(rval), alias_expanded);
              rval = append(cons(alias, cons(CDR(rval), C_NIL)));
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
  if (TYPEOF(prompt) != STRING)
    return;
  else
    {
      s = GETSTR(prompt);
      for (i = 0; s[i]; i++)
        {
          if (s[i] == '!')
            {
              sprintf(buf, "%d", INTVAL(histnum));
              strcat(current_prompt, buf);
              continue;
            }
          else if (s[i] == '\\')
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
  while (1)
    {
      brkflg = 0;
      interrupt = 0;
      printit = 0;
      echoline = 0;
      if (beforeprompt != NULL)
        (*beforeprompt)();
      /*
       * Evaluate promptform and print prompt.
       */
      if (options.interactive)
        {
          if (TYPEOF(eval(promptform)) == ERROR)
            {
              xprint(mkstring("Error in promptform, reset to nil"), C_T);
              promptform = C_NIL;
            }
          promptprint(*tprompt);
        }
      input_exp = xreadline(C_T);
      if (macrofun)
        switch ((*macrofun)(&input_exp))
          {
          case 0:
            return true;
          case 1:
            break;
          case 2:
            continue;
          }
      if (TYPEOF(input_exp) == ENDOFFILE)
        return true;
      if (EQ(CAR(input_exp), C_NIL))
        continue;
      addhist(input_exp);
      if (echoline)
        {
          prinbody(input_exp, stdout, 1);
          pputc('\n', primout);
        }
      topexp = transform(input_exp);
      if (TYPEOF(CAR(topexp)) == CONS)
        {
          topexp = CAR(topexp);
          printit = 1;
        }
      alias_expanded = C_NIL;
      topexp = eval(topexp);
      if (printit)
        xprint(topexp, C_T);
      if (!options.interactive && options.command)
        return false;
      trimhist();
    }
}

void init_hist()
{
  initcvar(&history, "history", C_NIL);
  initcvar(&histnum, "histnum", mknumber(1L));
  initcvar(&histmax, "histmax", mknumber(100L));
  mkprim(PN_PRINTHIST, printhist, 0, FSUBR);
}
