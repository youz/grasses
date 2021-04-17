#!/usr/bin/env ruby
#
# grass.rb - Grass interpreter
# http://www.blue.sky.or.jp/grass/
#
# Copyright (C) 2020 Yousuke Ushiki All rights reserved.
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

  App = Struct.new(:fun, :arg)
  Abs = Struct.new(:arity, :code)
  Fn = Struct.new(:arity, :code, :env)

  Succ = ->(c){ c.succ & 0xff }
  Out = ->(c){ putc(c); c }
  In = ->(eof){ ch = $stdin.getc; ch ? ch.ord : eof }
  ChurchTrue  = ->(x){->(y){ x }}
  ChurchFalse = ->(x){->(y){ y }}

  private

  def eval(code, env, dump)
    pc = 0
    while true
      if pc == code.length
        ret = env[0]
        if dump.empty?
          return ret
        end
        code, env, pc = dump.pop
        env = [ret] + env
      else
        insn = code[pc]
        val = case insn
        when App
          f, a = env[insn.fun-1], env[insn.arg-1]
          case f
          when Fn
            if f.arity == 1
              if pc < code.length - 1
                dump.push [code, env, pc+1]
              end
              code, env, pc = f.code, f.env, -1
              a
            else
              Fn.new(f.arity-1, f.code, [a] + f.env)
            end
          when Integer
            f == a ? ChurchTrue : ChurchFalse
          when Proc
            f[a]
          end
        when Abs
          Fn.new(insn.arity, insn.code, env)
        end
        env = [val] + env
        pc += 1
      end
    end
  end

  def parse(src)
    src.tr("\uFF37\uFF57\uFF56","Wwv").gsub(/[^Wwv]/, "").sub(/\A[^w]*/, "")
    .split(/v+/).map {|s|
      l = s.scan(/w+|W+/).map(&:size)
      arity = s[0] == 'w' ? l.shift : 0
      code = l.each_slice(2).map{|m, n|
        raise "syntax error" unless n
        App.new(m, n)
      }
      arity > 0 ? [Abs.new(arity, code)] : code
    }.flatten(1)
  end

  public

  def run(src)
    code = parse(src)
    e0 = [Out, Succ, 'w'.ord, In]
    d0 = [[[App.new(1, 1)], [], 0]]
    eval(code, e0, d0)
  end

end

if $0 == __FILE__ then
  if $*.length == 0
    puts "usage: ruby grass.rb <source.grass>"
    exit
  end
  Grass.new.run $<.read
end

