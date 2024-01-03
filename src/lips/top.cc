//
// Lips, lisp shell.
// Copyright 1988, 2020-2023 Krister Joas
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
#include "exec.hh"
#include "main.hh"
#include "top.hh"
#include "lips_error.hh"

using namespace lisp;

void top::print_history()
{
  for(auto hl: variables->history())
  {
    primout()->format("{}.\t", hl->car()->as_integer());
    prinbody(hl->cdr(), *vm::stdout(), io::escape::YES);
    primout()->terpri();
  }
}

void top::add_history(lisp_t what)
{
  variables->history() = cons(cons(variables->histnum(), what), variables->history());
  variables->histnum() = add1(variables->histnum());
}

void top::remove_history()
{
  variables->history() = variables->history()->cdr();
  variables->histnum() = sub1(variables->histnum());
}

void top::trim_history()
{
  lisp_t hl = variables->history();
  for(int i = 0; i < variables->histmax()->as_integer() && !is_nil(hl); i++, hl = hl->cdr())
    ;
  if(!is_nil(hl))
    rplacd(hl, nil);
}

lisp_t top::get_history(integer_t::value_type num, lisp_t hlist)
{
  if(num < 0)
  {
    for(; type_of(hlist) == object::type::Cons && num < 0; hlist = hlist->cdr(), num++)
      ;
    if(is_nil(hlist))
      return nil;
    return hlist->car()->cdr();
  }
  if(num > 0)
  {
    for(; type_of(hlist) == object::type::Cons && num != hlist->car()->car()->as_integer(); hlist = hlist->cdr())
      ;
    if(is_nil(hlist))
      return nil;
    return hlist->car()->cdr();
  }
  if(is_nil(hlist))
    return nil;
  return hlist->car()->cdr();
}

lisp_t top::printhist()
{
  remove_history(); // Removes itself from history.
  print_history();
  return nil;
}

lisp_t top::transform(lisp_t list)
{
  if(transform_hook)
    return transform_hook(list);
  return list;
}

lisp_t top::findalias(lisp_t exp)
{
  auto rval = exp;
  while(true)
  {
    if(type_of(rval) == object::type::Cons && type_of(rval->car()) == object::type::Symbol)
    {
      auto alias = getprop(rval->car(), C_ALIAS);
      if(!is_nil(alias) && (is_nil(alias_expanded) || rval->car() != alias_expanded->car()))
      {
        if(!is_nil(memb(rval->car(), alias_expanded)))
          throw lisp_error(lips_errc::alias_loop);
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

void top::set_prompt(lisp_t prompt)
{
  _current_prompt.clear();
  if(type_of(prompt) != object::type::String)
    return;
  auto s = prompt->getstr();
  for(auto c: s)
  {
    if(c == '!')
    {
      _current_prompt += std::to_string(top::variables->histnum()->as_integer());
      continue;
    }
    if(c == '\\')
      continue;
    _current_prompt.push_back(c);
  }
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
lisp_t top::operator()(lisp_t)
{
  while(true)
  {
    _echoline = false;
    if(prompt_hook)
      prompt_hook();
    //
    // Evaluate promptform and print prompt.
    //
    if(_options.interactive)
    {
      if(eval(variables->promptform()) == C_ERROR)
      {
        print(mkstring("Error in promptform, reset to nil"), T);
        variables->promptform() = nil;
      }
      set_prompt(variables->prompt());
    }
    input_exp = _terminal->readline(_current_prompt);
    if(input_exp == C_EOF)
      return nil;
    if(is_nil(input_exp))
      continue;
    if(is_nil(input_exp->car()))
      continue;
    top::add_history(input_exp);
    if(_echoline)
    {
      prinbody(input_exp, *primout(), io::escape::YES);
      primout()->terpri();
    }
    bool printit = false; // If the result will be printed.
    lisp_t topexp = transform(input_exp);
    if(type_of(topexp->car()) == object::type::Cons)
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
    top::trim_history();
  }
  return nil;
}

lisp_t top::rmexcl(lisp_t stream)
{
  auto c = stream->file()->getch();
  if(std::isspace(c) != 0)
    return C_EXCL;
  _echoline = true;
  lisp_t tmp = get_history(0, variables->history());
  switch(c)
  {
    case '!':
      return tmp;
      break;
    case '$':
      while(type_of(tmp->cdr()) == object::type::Cons)
        tmp = tmp->cdr();
      return tmp;
      break;
    case '*':
      return tmp->cdr();
      break;
    case '\n':
      _echoline = false;
      return C_EXCL;
      break;
    default:
      stream->file()->ungetch(c);
      auto at = ratom(stream);
      if(type_of(at) == object::type::Integer)
      {
        tmp = get_history(at->as_integer(), variables->history());
        return tmp;
      }
      if(type_of(at) == object::type::Symbol)
      {
        for(auto h: variables->history())
        {
          tmp = h->cdr();
          if(strncmp(tmp->car()->getstr().c_str(), at->getstr().c_str(), std::strlen(at->getstr().c_str())) == 0)
            return tmp;
        }
        return nil;
      }
      error(lips_errc::event_not_found, at);
      return nil;
  }
  return nil;
}

bool top::_echoline = false; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

namespace lisp::pn
{
inline constexpr auto PRINTHIST = "??";  // print history
inline constexpr auto RMEXCL = "rmexcl"; // History read-macro
} // namespace lisp::pn

void top::init()
{
  variables = std::make_unique<cvariables>();
  mkprim(
    pn::PRINTHIST, []() -> lisp_t { return top::printhist(); }, subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::RMEXCL, top::rmexcl, subr_t::subr::EVAL, subr_t::spread::SPREAD);
}

// NOLINTBEGIN(cppcoreguidelines-avoid-non-const-global-variables)
lisp_t top::input_exp; // The input expression.
std::function<lisp_t(lisp_t)> top::transform_hook;
std::function<void()> top::prompt_hook;
lisp_t top::alias_expanded;
std::unique_ptr<top::cvariables> top::variables;
// NOLINTEND(cppcoreguidelines-avoid-non-const-global-variables)

const lisp_t C_ALIAS = intern("alias");
const lisp_t C_EXCL = intern("!");
