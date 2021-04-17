/*
 * grass.ts - Grass interpreter
 * https://github.com/youz/grasses
 *
 * Copyright (C) 2020-2021 Yousuke Ushiki  <citrus.yubeshi@gmail.com>
 * All rights reserved.
 *
 * Grass language
 * http://www.blue.sky.or.jp/grass/
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */


import { readAllSync } from "https://deno.land/std/io/util.ts"

type Insn = App | Abs;

class App {
  constructor(
    readonly fun: number,
    readonly arg: number
  ) {}
}

class Abs {
  constructor(
    readonly arity: number,
    readonly code: Insn[]
  ) {}
}

class Env {
  declare readonly car: Value;
  declare readonly cdr: Env | null;
  constructor(car: Value, cdr: Env | null) {
    this.car = car;
    this.cdr = cdr;
  }

  cons(a: Value) {
    return new Env(a, this);
  }

  nth(n: number): Value {
    if (n === 0) {
      return this.car;
    } else if (this.cdr === null) {
      throw new Error("Env stack underflow");
    } else {
      return this.cdr.nth(n - 1);
    }
  }

  toString(): string {
    let buf = "[";
    let e: Env;
    e = this;
    while (e.cdr !== null) {
      buf += e.car.toString() + ";";
      e = e.cdr;
    }
    return buf + "]";
  }

  static create = (v: Value, ...rest: Value[]): Env => {
    let e: Env = new Env(v, null);
    for (const x of rest) e = new Env(x, e);
    return e;
  };
}

type Value = Prim | number | Fn;
type Prim = (arg: Value) => Value;
class Fn {
  declare readonly arity: number;
  declare readonly code: Insn[];
  declare readonly env: Env;
  constructor(arity: number, code: Insn[], env: Env) {
    this.arity = arity;
    this.code = code;
    this.env = env;
  }
}

const ctrue: Prim = (x: Value): Prim => (_: Value) => x;
const cfalse: Prim = (_: Value): Prim => (y: Value) => y;

const primSucc: Prim = (v: Value) => {
  if (typeof v !== "number") {
    throw new Error(`Succ: ${v} is not a char`);
  }
  return (v + 1) & 0xff;
};

function makeOut(wr: (n: number) => void): Prim {
  return (v: Value) => {
    if (typeof v !== "number") {
      throw new Error(`Out: ${v} is not a char`);
    }
    wr(v);
    return v;
  };
}

function makeIn(rr: () => number): Prim {
  return (eof: Value) => {
    const r = rr();
    return r < 0 ? eof : r;
  };
}

type Dump = { code: Insn[]; pc: number; env: Env };

function evalCode(code: Insn[], env: Env, dump: Dump[]): Value {
  let pc = 0;
  while (true) {
    if (pc === code.length) {
      const ret = env.nth(0);
      if (dump.length === 0) {
        return ret;
      }
      const d = dump[dump.length - 1];
      dump.pop();
      code = d.code;
      pc = d.pc;
      env = d.env.cons(ret);
    } else {
      const insn = code[pc];
      if (insn instanceof App) {
        const f = env.nth(insn.fun);
        const a = env.nth(insn.arg);
        switch (typeof f) {
          case "object":
            if (f.arity === 1) {
              if (pc < code.length - 1) {
                dump.push({ code: code, pc: pc + 1, env: env });
              }
              code = f.code;
              pc = 0;
              env = f.env.cons(a);
            } else {
              env = env.cons(new Fn(f.arity - 1, f.code, f.env.cons(a)));
              pc++;
            }
            break;
          case "number":
            env = env.cons(f === a ? ctrue : cfalse);
            pc++;
            break;
          case "function":
            env = env.cons(f(a));
            pc++;
            break;
          default:
            throw new Error(`unknown value: ${f}`);
        }
      } else {
        env = env.cons(new Fn(insn.arity, insn.code, env));
        pc++;
      }
    }
  }
}

const parseApps = (s: string) => {
  const buf: App[] = [];
  while (s !== "") {
    const m = s.match(/^(W+)(w+)(.*)/);
    if (m === null) {
      throw new Error(`syntax error at : ${s}`);
    }
    buf.push(new App(m[1].length - 1, m[2].length - 1));
    s = m[3];
  }
  return buf;
};

const Parse = (src: string) => {
  src = src.replace(/\uff37/g, "W").replace(/\uff57/g, "w").replace(
    /\uff56/g,
    "v",
  );
  src = src.replace(/[^Wwv]/g, "").replace(/^[^w]+/, "");
  let code: Insn[] = [];
  for (const s of src.split("v")) {
    if (s === "") {
      continue;
    } else if (s[0] === "w") {
      let arity = s.indexOf("W");
      if (arity < 0) arity = s.length;
      code.push(new Abs(arity, parseApps(s.substr(arity))));
    } else {
      code = code.concat(parseApps(s));
    }
  }
  return code;
};

function RunGrass(
  src: string,
  input: () => number,
  output: (n: number) => void,
) {
  const primIn = makeIn(input);
  const primOut = makeOut(output);
  const e0 = Env.create(primIn, 119, primSucc, primOut);
  const d0 = [{ code: [new App(0, 0)], pc: 0, env: e0 }];
  const code = Parse(src);
  evalCode(code, e0, d0);
}

function main(): number {
  if (Deno.args.length === 0) {
    console.log("usage: deno run --allow-read grass.ts <source.grass>");
    return 1;
  }

  const utf8dec = new TextDecoder("utf-8");
  const src = utf8dec.decode(Deno.readFileSync(Deno.args[0]));

  let inbuf: number[] = [];
  let read = false;
  const input = (): number => {
    if (!read) {
      const r = readAllSync(Deno.stdin);
      if (r !== null) {
        inbuf = Array.from(r);
      }
      read = true;
    }
    return inbuf.shift() || -1;
  };

  const outbuf: number[] = [];
  const output = (n: number) => outbuf.push(n);

  try {
    RunGrass(src, input, output);
    Deno.stdout.writeSync(new Uint8Array(outbuf));
  } catch (e) {
    console.error(e);
    return 2;
  }
  return 0;
}

Deno.exit(main());
