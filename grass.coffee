#
# grass.coffee - Grass interpreter for CoffeeScript
# https://github.com/youz/grasses
#
# Copyright (C) 2011 Yousuke Ushiki  <citrus.yubeshi@gmail.com>
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

class Grass
  constructor: (src)->
    @code = Grass.parse(src)

  # classes
  class App then constructor: (@m, @n) ->
  class Fn then constructor: (@code) ->
  
  # primitives
  read = (arg) ->
    @stdin.charCodeAt(@stdin_pos++) || arg

  write = (arg)->
    if arg.constructor is Number
      @stdout += String.fromCharCode(arg)
      arg
    else
      throw "not a char : #{arg}"

  succ = (arg)->
    if arg.constructor is Number
      arg + 1 & 255
    else
      throw "not a char : #{arg}"

  ctrue  = (x)-> (y)-> x # -> _eval([new Fn([new App(2, 1)])], [it, -> _eval([], [it]) ])
  cfalse = (x)-> (y)-> y # -> _eval([new Fn([])], [it])

  # evaluator
  _eval = (code, env)->
    e = env
    for insn in code
      val = switch insn.constructor
        when App
          fun = e[insn.m]
          arg = e[insn.n]
          switch fun.constructor
            when Function
              fun(arg)
            when Number
              if fun is arg then ctrue else cfalse
            else
              throw "bad state:\n  insn: #{insn}\n  env: #{e}"
        when Fn
          ((i, d)->
            (arg)-> _eval(i.code, [arg].concat(d))
            )(insn, e)
      e = [val].concat(e)
    e[0]

  # parser
  @parse = (src)->
    src = src.replace('\uFF37', 'W').replace('\uFF57', 'w').
           replace('\uFF56', 'v').replace(/^[^w]+/, "")
    code = []
    for s in src.split(/v/)
      s = s.match(/W*w+/g)
      arity = if s[0][0] == 'w' then s.shift().length else 0
      c = for ap in s
        [_, f, v] = ap.match(/^(W+)(w+)$/)
        new App(f.length-1, v.length-1)
      while arity-- > 0 then c = [new Fn(c)]
      code = code.concat(c)
    code

  run: ->
    @stdin = arguments[0] || ''
    @stdin_pos = 0
    @stdout = ""
    env = [((a)=> write.call(@, a)), succ, 119, ((a)=> read.call(@, a))]
    _eval([], [_eval([new App(0, 0)], [_eval(@code, env)])])
    @stdout

usage = -> console.log "Usage: coffee grass.coffee source.www"

if module.id is '.'
  if fn = process.argv[2]
    fs = require('fs')
    src = fs.readFileSync(fn, 'utf8')
    size = fs.fstatSync(process.stdin.fd).size
    input = if size > 0 then fs.readSync(process.stdin.fd, size)[0] else ''
    console.log new Grass(src).run(input)
    process.exit()
  else
    usage()
else
  module.exports = Grass
