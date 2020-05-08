/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include <cstring>

#include <libisp.hh>
#include <except.hh>
#include "main.hh"
#include "top.hh"

extern lisp::lisp* L;

using namespace lisp;

inline constexpr int PROMPTLENGTH = 80;

char current_prompt[PROMPTLENGTH];
LISPT input_exp; /* The input expression. */

/*
 * History functions.
 */
/*
 * Print the history list.
 */
void top::phist()
{
  LISPT hl;

  for(hl = history; !is_NIL(hl); hl = hl->cdr())
  {
    printf("%d.\t", hl->car()->car()->intval());
    prinbody(*L, hl->car()->cdr(), L->stdout(), true);
    L->primout().terpri();
  }
}

/*
 * Add event to history list.
 */
void top::addhist(LISPT what)
{
  history = cons(*L, cons(*L, histnum, what), history);
  histnum = add1(*L, histnum);
}

/*
 * Remove last event from history list.
 */
void top::remhist()
{
  history = history->cdr();
  histnum = sub1(*L, histnum);
}

/*
 * Trim history list to keep it shorter than histmax.
 */
void top::trimhist()
{
  LISPT hl = history;
  for(int i = 0; i < histmax->intval() && !is_NIL(hl); i++, hl = hl->cdr())
    ;
  if(!is_NIL(hl))
    rplacd(*L, hl, C_NIL);
}

/*
 * Return the NUM entry from history list HLIST, or nil if there is
 * no entry.
 */
LISPT top::histget(int num, LISPT hlist)
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

PRIMITIVE top::printhist()
{
  remhist(); /* Removes itself from history. */
  phist();
  return C_NIL;
}

LISPT top::transform(LISPT list)
{
  if(transformhook != nullptr)
    return (*transformhook)(list);
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
LISPT top::findalias(LISPT exp)
{
  auto rval = exp;
  while(true)
  {
    if(type_of(rval) == CONS && type_of(rval->car()) == SYMBOL)
    {
      auto alias = getprop(*L, rval->car(), C_ALIAS);
      if(!is_NIL(alias) && (is_NIL(alias_expanded) || !EQ(rval->car(), alias_expanded->car())))
      {
        if(!is_NIL(memb(*L, rval->car(), alias_expanded)))
          throw lisp_error("Alias loop");
        alias_expanded = cons(*L, rval->car(), alias_expanded);
        rval = append(*L, cons(*L, alias, cons(*L, rval->cdr(), C_NIL)));
      }
      else
        break;
    }
    else
      break;
  }
  return transform(rval);
}

void top::promptprint(LISPT prompt)
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
        sprintf(buf, "%d", top::histnum->intval());
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

bool top::toploop(LISPT* tprompt, int (*macrofun)(LISPT*), file_t& file)
{
  while(true)
  {
    L->echoline = false;
    if(beforeprompt != nullptr)
      (*beforeprompt)();
    //
    // Evaluate promptform and print prompt.
    //
    if(options.interactive)
    {
      if(type_of(eval(*L, promptform)) == ERROR)
      {
        xprint(*L, mkstring(*L, "Error in promptform, reset to nil"), C_T);
        promptform = C_NIL;
      }
      promptprint(*tprompt);
    }
    input_exp = readline(*L, file);
    if(is_NIL(input_exp))
      continue;
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
    top::addhist(input_exp);
    if(L->echoline)
    {
      prinbody(*L, input_exp, L->stdout(), 1);
      L->primout().terpri();
    }
    bool printit = false; // If the result will be printed.
    LISPT topexp = transform(input_exp);
    if(type_of(topexp->car()) == CONS)
    {
      topexp = topexp->car();
      printit = true;
    }
    alias_expanded = C_NIL;
    topexp = eval(*L, topexp);
    if(printit)
      xprint(*L, topexp, C_T);
    if(!options.interactive && options.command)
      return false;
    top::trimhist();
  }
}

/*
 * Redo read macro:
 *   !!      - last command
 *   !-n     - the n'th previous command
 *   !n      - command n
 *   !s      - command with prefix s
 *   !$      - last argument
 *   !*      - all arguments
 * others could be added easily.
 */
LISPT top::rmexcl(lisp& l, file_t& file, LISPT, char)
{
  LISPT at;

  int c = file.getch();
  if(issepr(l, c))
    return C_EXCL;
  l.echoline = true;
  LISPT tmp = histget(0L, history);
  if(type_of(tmp->car()) == CONS && is_NIL(tmp->cdr()))
    tmp = tmp->car();
  switch(c)
  {
    case '!':
      return histget(0L, history);
      break;
    case '$':
      while(type_of(tmp->cdr()) == CONS) tmp = tmp->cdr();
      return tmp;
      break;
    case '*':
      return tmp->cdr();
      break;
    case '\n':
      l.echoline = false;
      return C_EXCL;
      break;
    default:
      file.ungetch(c);
      at = io(l).ratom(file);
      if(type_of(at) == INTEGER)
      {
        tmp = histget(at->intval(), history);
        return tmp;
      }
      if(type_of(at) == SYMBOL)
      {
        for(auto h = history; !is_NIL(h); h = h->cdr())
        {
          tmp = histget(0L, h);
          if(!is_NIL(tmp) && type_of(tmp->car()) == CONS && is_NIL(tmp->cdr()))
            tmp = tmp->car();
          if(!strncmp(tmp->car()->getstr(), at->getstr(), std::strlen(at->getstr())))
            return histget(0L, h);
        }
        return C_NIL;
      }
      else
      {
        l.error(EVENT_NOT_FOUND, at);
        return C_NIL;
      }
  }
  return C_NIL;
}

void top::init()
{
  L->a().add_mark_object(&top::history);
  L->a().add_mark_object(&top::histnum);
  L->a().add_mark_object(&top::histmax);
  L->a().add_mark_object(&alias_expanded);
  L->a().add_mark_object(&promptform);
  initcvar(&top::history, "history", C_NIL);
  initcvar(&top::histnum, "histnum", mknumber(*L, 1L));
  initcvar(&top::histmax, "histmax", mknumber(*L, 100L));
  initcvar(&promptform, "promptform", C_NIL);
  mkprim(PN_PRINTHIST, [](lisp&) -> LISPT { return top::printhist(); }, 0, FSUBR);
  L->set_read_table('!', SPLICE, top::rmexcl);
}

LISPT top::history = nullptr;
LISPT top::histnum = nullptr;
LISPT top::histmax = nullptr;
LISPT (*top::transformhook)(LISPT) = nullptr;
void (*top::beforeprompt)() = nullptr;
LISPT top::alias_expanded = nullptr;
LISPT top::promptform = nullptr;
