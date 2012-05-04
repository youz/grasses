#
# grass.cy - Grass compiler for Cyan language
# https://github.com/youz/grasses
#
# Copyright (C) 2008 Yousuke Ushiki  <citrus.yubeshi@gmail.com>
# All rights reserved.
#
# Grass language
# http://www.blue.sky.or.jp/grass/
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in
#    the documentation and/or other materials provided with the
#    distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
# THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS
# BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
# BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
# OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
# IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# History:
#
# 2008-11-07
#   - First version for Cyan ver 1.0.2
#
#
# Usage:
# read grass-code from stdin
#  $ cyan grass.cy < hello.www
#
# read grass-code from command line
#  $ cyan grass.cy -e wWWwwwwWWww
#
# read input from command line
#  $ cyan grass.cy -i asdf < echo.www
#

## Utilities
def(Object.list?)^: .parent == List || .parent == Pair
def(List.car)^: []
def(List.cadr)^: .cdr().car()
def(List.cddr)^: .cdr().cdr()
def(List.caddr)^: .cdr().cdr().car()

def(Stream.read)^:
  let(rec)^(&opt buf = ""):
    aif (.readline()): rec(buf + it)
     else: buf

mac(catch)^(label,body):
  sym = Symbol.generate()
  `callcc^(?sym):
    ?label.eval() = ?sym
    *(?body.list)

def(throw)^(label, v):
  label.eval()(v)

def(grass_err)^(sym, msg, v):
  stderr.writeline("grass_" + sym.to_s() + ": " + msg)
  throw(sym, v)


## Char-map
$(code_char, char_code) = &(%{}, %{})
for($(i, c, s) := &(32, [], " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~.".istring()),
  c = s.readc(),++i):
    $(code_char[i], char_code[c]) = &(c, i)

[[9|"\t"], [10|"\n"], [13|"\r"]].map^(p):
  $(code_char[p.car()], char_code[p.cdr()]) = &(p.cdr(), p.car())


## Parser
def(grass_parse_string)^(str):
  s := str.istring()
  grass_char := %{ "w" => 0, "ｗ" => 0, "W" => 1, "Ｗ" => 1, "v" => 2, "V" => 2, "ｖ" => 2, "Ｖ" => 2 }
  catch('parse_error){
    loop:
      aif (s.readc()):
        if (grass_char[it] == 0): break()
       else:
        grass_err('parse-error, "No green", [])
    let(rec)^(&opt acc, apps = ['abs], last = 0, len = 1):
      aif (s.readc()):
        aif (grass_char[it]):
          cond:
            (last.null?()):
              cond:
                (it == 0): rec(acc, ['abs], it, 1)
                (it == 1): rec(acc, ['app], it, 1)
                else:  grass_err('parse_error,"0 length abstraction",[])
            (it == last):
              rec(acc, apps, last, len + 1)
            (it == 2):
              if (last == 1):
                grass_err('parse_error, "Malformed application", [])
               else:
                rec([[len | apps].reverse() | acc], [], [])
            else:
              rec(acc, [len | apps], it, 1)
         else:
          rec(acc, apps, last, len)
       else:
        [[len | apps].reverse() | acc]
  }.foldl([])^(acc, l):
    rec := ^(apps, acc):
     if (apps.null?()):
       acc.reverse()
      else:
       rec(apps.cddr(), [['app, apps.car(), apps.cadr()] | acc])
    if (l.car() == 'app):
      [*rec(l.cdr(), []) | acc]
     else:
      [['abs, l.cadr(), rec(l.cddr(), [])] | acc]


## Compiler
def(grass_compile_code)^(code, d, &opt e):
  $(tag, arity, body) := &(code.car(), code.cadr(), code.caddr())

  catch('compile_error):
    if (tag != 'abs || arity.parent != Int || !body.list?()):
      grass_err('compile_error, "Malformed abstraction code\n  " + code.to_s(), [code,d,e])

    if (arity > 0):
      let^(&opt arg = Symbol.generate()):
        `^(?arg){ ?grass_compile_code(['abs, arity - 1, body], [arg | d], false)}
     else:
      $(tag, sym) := &('app, d[0])
      `begin:
        ?*body.map^(a):
          $(tag, sym) = &(a.car(), Symbol.generate())
          push!(d, sym)
          cond:
            (tag == 'app):
              `(?sym := ?Porter.new(d[a.cadr()], &(d[a.caddr()])))
            (tag == 'abs):
              aif (grass_compile_code(a, d.cdr(), false)):
                `(?sym := ?it)
            else:
              grass_err('compile_error, "Malformed code\n  " + a.to_s(), [])
        ?if (e && (tag == 'abs)):
          Porter.new(sym, &(sym))
         else:
          sym

## Primitives
def(mkchar)^(code):
  ^(&opt ch):
    cond:
      (ch.null?()):   code
      (ch() == code): ^(x): ^(y): x
      else:           ^(x): ^(y): y

def(mkin)^(s):
  ^(eof):
    aif(s.readc()): mkchar(char_code[it] || 0)
     else: eof

def(mkout)^(s):
  ^(ch): s.write(code_char[ch()]); ch

## Eval
def(grass_eval_string)^(str, &key output = stdout, input = stdin, run = true):
  aif (grass_parse_string(str)):
    grass_eval_code(['abs, 0, it], :output output, :input input, :run run)

def(grass_eval_code)^(code, &key output = stdout, input = stdin, run = true):
  begin:
    out := mkout(output)
    succ := ^(ch){ mkchar((1 + ch()).rem(256)) }
    w := mkchar(119)
    in := mkin(input)
    grass_compile_code(code, '[out, succ, w, in], run).eval()


## Command line execution
if (__FILE__ == PROGRAM_NAME):
  let(rec)^(&opt a = ARGS, is = stdin, code):
    if (a.null?()):
      if (!code): code = stdin.read()
      grass_eval_string(code, :input is)
      stderr.write("\n")
     else:
      as := a[0].istring()
      if (as.readc() == "-"):
        opt := as.readc()
        cond:
          (opt == "e"): rec(a.cddr(), is, a[1].istring().read())
          (opt == "i"): rec(a.cddr(), a[1].istring(), code)
