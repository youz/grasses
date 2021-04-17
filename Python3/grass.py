#!/usr/bin/env python3
#
# grass.py - Grass interpreter
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


import sys
import string
import re
from itertools import islice, groupby
from typing import List, Union, Callable, Tuple

Insn = Union['App', 'Abst']
Value = Union[int, 'Fn', Callable[['Value'], 'Value']]

class App:
    def __init__(self, fun: int, arg: int):
        self.fun = fun
        self.arg = arg

    def __str__(self):
        return f"App({self.fun}, {self.arg})"

class Abst:
    def __init__(self, arity: int):
        self.arity = arity
        self.codelen = 0
        self.code = []

    def append(self, insn: Insn):
        self.code.append(insn)
        self.codelen += 1
    
    def __str__(self):
        return "Abs(arity:%d, len:%d, code:[%s])" % (self.arity, self.codelen, str.join(',', map(str, self.code)))

class Fn:
    def __init__(self, abst: Abst, env: List[Value]):
        self.arity = abst.arity
        self.abst = abst
        self.env = env

    def bind(self, a: Value) -> 'Fn':
        f = Fn(self.abst, [a] + self.env)
        f.arity = self.arity - 1
        return f
    
    def __str__(self):
        return f"Fn(arity:{self.arity}, abst:{self.abst})"

ctrue = lambda x: lambda y: x
cfalse = lambda x: lambda y: y

def print_env(e: List[Value]):
    print("--Env--")
    i = 1
    for v in e:
        print(f"{i}: {v}")
        i += 1

Dump = Tuple[List[Insn], List[Value]]
def eval_code(code: List[Insn], env: List[Value], dump: List[Dump]) -> Value:
    while True:
        if code == []:
            ret = env[0]
            if dump == []: return ret
            code, env = dump.pop()
            env = [ret] + env
        else:
            insn = code[0]
            code = code[1:]
            if type(insn) == App:
                f = env[insn.fun - 1]
                a = env[insn.arg - 1]
                if type(f) == int:
                    if f == a:
                        b = ctrue
                    else:
                        b = cfalse
                    env = [b] + env
                elif callable(f):
                    try:
                        env = [f(a)] + env
                    except Exception:
                        print_env(env)
                        print(f"error occurred on {insn}")
                        raise
                elif type(f) == Fn:
                    if f.arity == 1:
                        if code != []:
                            dump.append((code, env))
                        code = f.abst.code
                        env = [a] + f.env
                    else:
                        env = [f.bind(a)] + env
                else:
                    raise Exception(f"unknown value type: {f}")
            else:
                env = [Fn(insn, env)] + env


def parse_app(tokens):
    if tokens[0][0] != 'W' or tokens[1][0] != 'w':
        raise Exception("syntax error")
    fun = tokens[0][1]
    arg = tokens[1][1]
    return (App(fun, arg), tokens[2:])

def parse_abst(tokens):
    abst = Abst(tokens[0][1])
    tokens.pop(0)
    while len(tokens) > 0 and tokens[0][0] != 'v':
        app, tokens = parse_app(tokens)
        abst.append(app)
    return (abst, tokens[1:])

def parse(src: str) -> Abst:
    src = src.replace('ｗ','w').replace('Ｗ','W').replace('ｖ','v')
    src = re.sub('[^wWv]', '', src)
    src = re.sub('^[^w]+', '', src)
    tokens = [[s[0], len(list(i))] for s, i in groupby(src)]
    prog = Abst(0)
    while len(tokens) > 0:
        c = tokens[0][0]
        if c == 'w':
            abst, tokens = parse_abst(tokens)
            prog.append(abst)
        elif c == 'W':
            app, tokens = parse_app(tokens)
            prog.append(app)
        elif c == 'v':
            tokens.pop(0)
    return prog

def prim_out(v: Value) -> Value:
    if type(v) == int:
        sys.stdout.write(chr(v))
        return v
    else:
        raise Exception(f"Out: {v} is not a char")

def prim_succ(v: Value) -> Value:
    if type(v) == int:
        return (v + 1) % 256
    else:
        raise Exception(f"Succ: {v} is not a char")

def prim_in(eof: Value) -> Value:
    c = sys.stdin.read(1)
    if c == "":
        return eof
    else:
        return ord(c)


def run(src: str):
    prog = parse(src)
    e0 = [prim_out, prim_succ, 119, prim_in]
    d0 = [([App(1,1)],[])]
    eval_code(prog.code, e0, d0)


if __name__ == '__main__':
    if len(sys.argv) == 1:
        print(f"usage: python {__file__} <program.grass>")
    else:
        try:
            run(str.join('', open(sys.argv[1]).readlines()))
        except Exception as e:
            print(e)
            exit(1)
