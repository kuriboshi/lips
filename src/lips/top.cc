/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include <cstring>

#include <fmt/format.h>
#include <iostream>
#include <lisp/libisp.hh>
#include <lisp/except.hh>
#include "main.hh"
#include "top.hh"

using namespace lisp;

std::string current_prompt;
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
    std::cout << fmt::format("{}.\t", hl->car()->car()->intval());
    prinbody(hl->car()->cdr(), lisp::current().stdout(), true);
    primout().terpri();
  }
}

/*
 * Add event to history list.
 */
void top::addhist(LISPT what)
{
  history = cons(cons(histnum, what), history);
  histnum = add1(histnum);
}

/*
 * Remove last event from history list.
 */
void top::remhist()
{
  history = history->cdr();
  histnum = sub1(histnum);
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
    rplacd(hl, C_NIL);
}

/*
 * Return the NUM entry from history list HLIST, or nil if there is
 * no entry.
 */
LISPT top::histget(int num, LISPT hlist)
{
  if(num < 0)
  {
    for(; type_of(hlist) == lisp_type::CONS && num < 0; hlist = hlist->cdr(), num++)
      ;
    if(is_NIL(hlist))
      return C_NIL;
    else
      return hlist->car()->cdr();
  }
  else if(num > 0)
  {
    for(; type_of(hlist) == lisp_type::CONS && num != hlist->car()->car()->intval(); hlist = hlist->cdr())
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
  if(transform_hook)
    return transform_hook(list);
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
    if(type_of(rval) == lisp_type::CONS && type_of(rval->car()) == lisp_type::SYMBOL)
    {
      auto alias = getprop(rval->car(), C_ALIAS);
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

void top::promptprint(LISPT prompt)
{
  current_prompt.clear();
  if(type_of(prompt) != lisp_type::STRING)
    return;
  else
  {
    auto s = prompt->getstr();
    for(auto c: s)
    {
      if(c == '!')
      {
        current_prompt += std::to_string(top::histnum->intval());
        continue;
      }
      else if(c == '\\')
        continue;
      current_prompt.push_back(c);
    }
  }
  std::cout << "\r";
  std::cout << current_prompt;
}

bool top::toploop(LISPT* tprompt, int (*macrofun)(LISPT*), file_t& file)
{
  while(true)
  {
    lisp::current().echoline = false;
    if(prompt_hook)
      prompt_hook();
    //
    // Evaluate promptform and print prompt.
    //
    if(options.interactive)
    {
      if(type_of(eval(promptform)) == lisp_type::ERROR)
      {
        print(mkstring("Error in promptform, reset to nil"), C_T);
        promptform = C_NIL;
      }
      promptprint(*tprompt);
    }
    input_exp = readline(file);
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
    if(type_of(input_exp) == lisp_type::ENDOFFILE)
      return true;
    if(EQ(input_exp->car(), C_NIL))
      continue;
    top::addhist(input_exp);
    if(lisp::current().echoline)
    {
      prinbody(input_exp, primout(), 1);
      primout().terpri();
    }
    bool printit = false; // If the result will be printed.
    LISPT topexp = transform(input_exp);
    if(type_of(topexp->car()) == lisp_type::CONS)
    {
      topexp = topexp->car();
      printit = true;
    }
    alias_expanded = C_NIL;
    topexp = eval(topexp);
    if(printit)
      print(topexp, C_T);
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
  if(type_of(tmp->car()) == lisp_type::CONS && is_NIL(tmp->cdr()))
    tmp = tmp->car();
  switch(c)
  {
    case '!':
      return histget(0L, history);
      break;
    case '$':
      while(type_of(tmp->cdr()) == lisp_type::CONS) tmp = tmp->cdr();
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
      if(type_of(at) == lisp_type::INTEGER)
      {
        tmp = histget(at->intval(), history);
        return tmp;
      }
      if(type_of(at) == lisp_type::SYMBOL)
      {
        for(auto h = history; !is_NIL(h); h = h->cdr())
        {
          tmp = histget(0L, h);
          if(!is_NIL(tmp) && type_of(tmp->car()) == lisp_type::CONS && is_NIL(tmp->cdr()))
            tmp = tmp->car();
          if(!strncmp(tmp->car()->getstr().c_str(), at->getstr().c_str(), std::strlen(at->getstr().c_str())))
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
  gcprotect(top::history);
  gcprotect(top::histnum);
  gcprotect(top::histmax);
  gcprotect(alias_expanded);
  gcprotect(promptform);
  initcvar(&top::history, "history", C_NIL);
  initcvar(&top::histnum, "histnum", mknumber(1L));
  initcvar(&top::histmax, "histmax", mknumber(100L));
  initcvar(&promptform, "promptform", C_NIL);
  mkprim(PN_PRINTHIST, [](lisp&) -> LISPT { return top::printhist(); }, subr_t::S_NOEVAL, subr_t::S_NOSPREAD);
  lisp::current().set_read_table('!', char_class::SPLICE, top::rmexcl);
}

LISPT top::history = nullptr;
LISPT top::histnum = nullptr;
LISPT top::histmax = nullptr;

std::function<LISPT(LISPT)> top::transform_hook;;
std::function<void()> top::prompt_hook;
LISPT top::alias_expanded = nullptr;
LISPT top::promptform = nullptr;
