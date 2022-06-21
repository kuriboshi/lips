//
// Lips, lisp shell.
// Copyright 1988, 2020-2022 Krister Joas
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

#include <cstring>

#include <fmt/format.h>
#include <iostream>
#include <lisp/lisp.hh>
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
    prinbody(hl->cdr(), *lisp::current().stdout(), true);
    primout()->terpri();
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
  if(num > 0)
  {
    for(; type_of(hlist) == type::CONS && num != hlist->car()->car()->intval(); hlist = hlist->cdr())
      ;
    if(is_NIL(hlist))
      return NIL;
    return hlist->car()->cdr();
  }
  if(!is_NIL(hlist))
    return hlist->car()->cdr();
  return NIL;
}

LISPT top::printhist()
{
  remhist(); /* Removes itself from history. */
  phist();
  return NIL;
}

LISPT top::transform(LISPT list)
{
  if(transform_hook)
    return transform_hook(l, list);
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
      if(!is_NIL(alias) && (is_NIL(alias_expanded) || rval->car() != alias_expanded->car()))
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
  auto s = prompt->getstr();
  for(auto c: s)
  {
    if(c == '!')
    {
      current_prompt += std::to_string(top::variables->histnum->intval());
      continue;
    }
    if(c == '\\')
      continue;
    current_prompt.push_back(c);
  }
  std::cout << "\r";
  std::cout << current_prompt;
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
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
    if(_options.interactive)
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
    input_exp = readline(_file);
    if(type_of(input_exp) == type::ENDOFFILE)
      return NIL;
    if(is_NIL(input_exp))
      continue;
    if(is_NIL(input_exp->car()))
      continue;
    top::addhist(input_exp);
    if(lisp::current().echoline)
    {
      prinbody(input_exp, *primout(), true);
      primout()->terpri();
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
    if(!_options.interactive && _options.command)
      return NIL;
    top::trimhist();
  }
  return NIL;
}

/// @brief Redo read macro.
///
/// @details
///  !!      - last command
///  !-n     - the n'th previous command
///  !n      - command n
///  !s      - command with prefix s
///  !$      - last argument
///  !*      - all arguments
/// others could be added easily.
LISPT top::rmexcl(lisp& l, LISPT stream)
{
  int c = stream->file()->getch();
  if(std::isspace(c) != 0)
    return C_EXCL;
  l.echoline = true;
  LISPT tmp = histget(0L, variables->history);
  switch(c)
  {
    case '!':
      return tmp;
      break;
    case '$':
      while(type_of(tmp->cdr()) == type::CONS)
        tmp = tmp->cdr();
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
      stream->file()->ungetch(c);
      auto at = io::ratom(l, stream->file());
      if(type_of(at) == type::INTEGER)
      {
        tmp = histget(at->intval(), variables->history);
        return tmp;
      }
      if(type_of(at) == type::SYMBOL)
      {
        for(auto h: variables->history)
        {
          tmp = h->cdr();
          if(strncmp(tmp->car()->getstr().c_str(), at->getstr().c_str(), std::strlen(at->getstr().c_str())) == 0)
            return tmp;
        }
        return NIL;
      }
      l.error(EVENT_NOT_FOUND, at);
      return NIL;
  }
  return NIL;
}

namespace lisp::pn
{
inline constexpr auto PRINTHIST = "??";  // print history
inline constexpr auto RMEXCL = "rmexcl"; // History read-macro
} // namespace lisp::pn

void top::init(alloc& a)
{
  variables = std::make_unique<cvariables>(a);
  mkprim(
    pn::PRINTHIST, [](lisp&) -> LISPT { return top::printhist(); }, subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::RMEXCL, top::rmexcl, subr_t::subr::EVAL, subr_t::spread::SPREAD);
}

LISPT top::input_exp; // The input expression.
std::function<LISPT(::lisp::lisp&, LISPT)> top::transform_hook;
std::function<void()> top::prompt_hook;
LISPT top::alias_expanded;
std::unique_ptr<top::cvariables> top::variables;
