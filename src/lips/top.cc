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

/*
 * History functions.
 */
/*
 * Print the history list.
 */
void top::phist()
{
  for(auto hl: variables->history)
  {
    std::cout << fmt::format("{}.\t", hl->car()->intval());
    prinbody(hl->cdr(), lisp::current().stdout(), true);
    primout().terpri();
  }
}

/*
 * Add event to history list.
 */
void top::addhist(LISPT what)
{
  variables->history = cons(cons(variables->histnum, what), variables->history);
  variables->histnum = add1(variables->histnum);
}

/*
 * Remove last event from history list.
 */
void top::remhist()
{
  variables->history = variables->history->cdr();
  variables->histnum = sub1(variables->histnum);
}

/*
 * Trim history list to keep it shorter than histmax.
 */
void top::trimhist()
{
  LISPT hl = variables->history;
  for(int i = 0; i < variables->histmax->intval() && !is_NIL(hl); i++, hl = hl->cdr())
    ;
  if(!is_NIL(hl))
    rplacd(hl, NIL);
}

/*
 * Return the NUM entry from history list HLIST, or nil if there is
 * no entry.
 */
LISPT top::histget(int num, LISPT hlist)
{
  if(num < 0)
  {
    for(; type_of(hlist) == type::CONS && num < 0; hlist = hlist->cdr(), num++)
      ;
    if(is_NIL(hlist))
      return NIL;
    return hlist->car()->cdr();
  }
  else if(num > 0)
  {
    for(; type_of(hlist) == type::CONS && num != hlist->car()->car()->intval(); hlist = hlist->cdr())
      ;
    if(is_NIL(hlist))
      return NIL;
    return hlist->car()->cdr();
  }
  else if(!is_NIL(hlist))
    return hlist->car()->cdr();
  return NIL;
}

PRIMITIVE top::printhist()
{
  remhist(); /* Removes itself from history. */
  phist();
  return NIL;
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
    if(type_of(rval) == type::CONS && type_of(rval->car()) == type::SYMBOL)
    {
      auto alias = getprop(rval->car(), C_ALIAS);
      if(!is_NIL(alias) && (is_NIL(alias_expanded) || !EQ(rval->car(), alias_expanded->car())))
      {
        if(!is_NIL(memb(rval->car(), alias_expanded)))
          throw lisp_error("Alias loop");
        alias_expanded = cons(rval->car(), alias_expanded);
        rval = append(cons(alias, cons(rval->cdr(), NIL)));
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
  if(type_of(prompt) != type::STRING)
    return;
  else
  {
    auto s = prompt->getstr();
    for(auto c: s)
    {
      if(c == '!')
      {
        current_prompt += std::to_string(top::variables->histnum->intval());
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

LISPT top::operator()(LISPT exp)
{
  ++_level;
  while(true)
  {
    l.echoline = false;
    if(prompt_hook)
      prompt_hook();
    //
    // Evaluate promptform and print prompt.
    //
    if(options.interactive)
    {
      if(type_of(eval(l, variables->promptform)) == type::ERROR)
      {
        print(mkstring(l, "Error in promptform, reset to nil"), T);
        variables->promptform = NIL;
      }
      if(_level > 1)
        promptprint(variables->brkprompt);
      else
        promptprint(variables->topprompt);
    }
    input_exp = readline(file);
    if(type_of(input_exp) == type::ENDOFFILE)
      return NIL;
    if(is_NIL(input_exp))
      continue;
    if(EQ(input_exp->car(), NIL))
      continue;
#if 0
    if(transform_hook)
    {
      switch(transform_hook(lisp, &input_exp))
      {
        case break_return::RETURN:
          return;
        case break_return::PROCEED:
          break;
        case break_return::SKIP:
          continue;
      }
    }
#endif
    top::addhist(input_exp);
    if(lisp::current().echoline)
    {
      prinbody(input_exp, primout(), 1);
      primout().terpri();
    }
    bool printit = false; // If the result will be printed.
    LISPT topexp = transform(input_exp);
    if(type_of(topexp->car()) == type::CONS)
    {
      topexp = topexp->car();
      printit = true;
    }
    alias_expanded = NIL;
    topexp = eval(topexp);
    if(printit)
      print(topexp, T);
    if(!options.interactive && options.command)
      return NIL;
    top::trimhist();
  }
  return NIL;
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
  LISPT tmp = histget(0L, variables->history);
  if(type_of(tmp->car()) == type::CONS && is_NIL(tmp->cdr()))
    tmp = tmp->car();
  switch(c)
  {
    case '!':
      return histget(0L, variables->history);
      break;
    case '$':
      while(type_of(tmp->cdr()) == type::CONS) tmp = tmp->cdr();
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
      if(type_of(at) == type::INTEGER)
      {
        tmp = histget(at->intval(), variables->history);
        return tmp;
      }
      if(type_of(at) == type::SYMBOL)
      {
        for(auto h: variables->history)
        {
          tmp = histget(0L, h);
          if(!is_NIL(tmp) && type_of(tmp->car()) == type::CONS && is_NIL(tmp->cdr()))
            tmp = tmp->car();
          if(!strncmp(tmp->car()->getstr().c_str(), at->getstr().c_str(), std::strlen(at->getstr().c_str())))
            return histget(0L, h);
        }
        return NIL;
      }
      else
      {
        l.error(EVENT_NOT_FOUND, at);
        return NIL;
      }
  }
  return NIL;
}

void top::init(alloc& a)
{
  variables = std::make_unique<cvariables>(a);
  mkprim(PN_PRINTHIST, [](lisp&) -> LISPT { return top::printhist(); }, subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  lisp::current().set_read_table('!', char_class::SPLICE, top::rmexcl);
}

LISPT top::input_exp;           // The input expression.
std::function<LISPT(LISPT)> top::transform_hook;;
std::function<void()> top::prompt_hook;
LISPT top::alias_expanded;
std::unique_ptr<top::cvariables> top::variables;
