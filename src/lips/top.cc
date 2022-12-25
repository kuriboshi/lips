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
#include "lips_error.hh"

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
    prinbody(hl->cdr(), *context::current().stdout(), true);
    primout()->terpri();
  }
}

/*
 * Add event to history list.
 */
void top::addhist(lisp_t what)
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
  lisp_t hl = variables->history;
  for(int i = 0; i < variables->histmax->intval() && !is_nil(hl); i++, hl = hl->cdr())
    ;
  if(!is_nil(hl))
    rplacd(hl, nil);
}

/*
 * Return the NUM entry from history list HLIST, or nil if there is
 * no entry.
 */
lisp_t top::histget(int num, lisp_t hlist)
{
  if(num < 0)
  {
    for(; type_of(hlist) == type::Cons && num < 0; hlist = hlist->cdr(), num++)
      ;
    if(is_nil(hlist))
      return nil;
    return hlist->car()->cdr();
  }
  if(num > 0)
  {
    for(; type_of(hlist) == type::Cons && num != hlist->car()->car()->intval(); hlist = hlist->cdr())
      ;
    if(is_nil(hlist))
      return nil;
    return hlist->car()->cdr();
  }
  if(!is_nil(hlist))
    return hlist->car()->cdr();
  return nil;
}

lisp_t top::printhist()
{
  remhist(); /* Removes itself from history. */
  phist();
  return nil;
}

lisp_t top::transform(lisp_t list)
{
  if(transform_hook)
    return transform_hook(_ctx, list);
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
lisp_t top::findalias(lisp_t exp)
{
  auto rval = exp;
  while(true)
  {
    if(type_of(rval) == type::Cons && type_of(rval->car()) == type::Symbol)
    {
      auto alias = getprop(rval->car(), C_ALIAS);
      if(!is_nil(alias) && (is_nil(alias_expanded) || rval->car() != alias_expanded->car()))
      {
        if(!is_nil(memb(rval->car(), alias_expanded)))
          throw lisp_error("Alias loop");
        alias_expanded = cons(rval->car(), alias_expanded);
        rval = append(cons(alias, cons(rval->cdr(), nil)));
      }
      else
        break;
    }
    else
      break;
  }
  return transform(rval);
}

void top::promptprint(lisp_t prompt)
{
  current_prompt.clear();
  if(type_of(prompt) != type::String)
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
lisp_t top::operator()(lisp_t exp)
{
  ++_level;
  while(true)
  {
    _ctx.echoline = false;
    if(prompt_hook)
      prompt_hook();
    //
    // Evaluate promptform and print prompt.
    //
    if(_options.interactive)
    {
      if(type_of(eval(variables->promptform)) == type::Error)
      {
        print(mkstring("Error in promptform, reset to nil"), T);
        variables->promptform = nil;
      }
      if(_level > 1)
        promptprint(variables->brkprompt);
      else
        promptprint(variables->topprompt);
    }
    input_exp = readline(_file);
    if(type_of(input_exp) == type::Eof)
      return nil;
    if(is_nil(input_exp))
      continue;
    if(is_nil(input_exp->car()))
      continue;
    top::addhist(input_exp);
    if(context::current().echoline)
    {
      prinbody(input_exp, *primout(), true);
      primout()->terpri();
    }
    bool printit = false; // If the result will be printed.
    lisp_t topexp = transform(input_exp);
    if(type_of(topexp->car()) == type::Cons)
    {
      topexp = topexp->car();
      printit = true;
    }
    alias_expanded = nil;
    topexp = eval(topexp);
    if(printit)
      print(topexp, T);
    if(!_options.interactive && _options.command)
      return nil;
    top::trimhist();
  }
  return nil;
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
lisp_t top::rmexcl(context& ctx, lisp_t stream)
{
  int c = stream->file()->getch();
  if(std::isspace(c) != 0)
    return C_EXCL;
  ctx.echoline = true;
  lisp_t tmp = histget(0L, variables->history);
  switch(c)
  {
    case '!':
      return tmp;
      break;
    case '$':
      while(type_of(tmp->cdr()) == type::Cons)
        tmp = tmp->cdr();
      return tmp;
      break;
    case '*':
      return tmp->cdr();
      break;
    case '\n':
      ctx.echoline = false;
      return C_EXCL;
      break;
    default:
      stream->file()->ungetch(c);
      auto at = io::ratom(stream->file());
      if(type_of(at) == type::Integer)
      {
        tmp = histget(at->intval(), variables->history);
        return tmp;
      }
      if(type_of(at) == type::Symbol)
      {
        for(auto h: variables->history)
        {
          tmp = h->cdr();
          if(strncmp(tmp->car()->getstr().c_str(), at->getstr().c_str(), std::strlen(at->getstr().c_str())) == 0)
            return tmp;
        }
        return nil;
      }
      ctx.error(lips_errc::event_not_found, at);
      return nil;
  }
  return nil;
}

namespace lisp::pn
{
inline constexpr auto PRINTHIST = "??";  // print history
inline constexpr auto RMEXCL = "rmexcl"; // History read-macro
} // namespace lisp::pn

void top::init()
{
  variables = std::make_unique<cvariables>();
  mkprim(
    pn::PRINTHIST, [](context&) -> lisp_t { return top::printhist(); }, subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::RMEXCL, top::rmexcl, subr_t::subr::EVAL, subr_t::spread::SPREAD);
}

lisp_t top::input_exp; // The input expression.
std::function<lisp_t(::lisp::context&, lisp_t)> top::transform_hook;
std::function<void()> top::prompt_hook;
lisp_t top::alias_expanded;
std::unique_ptr<top::cvariables> top::variables;
