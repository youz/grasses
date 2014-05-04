/*
 * grass.d - Grass interpreter
 * https://github.com/youz/grasses
 *
 * Copyright (C) 2014 Yousuke Ushiki  <citrus.yubeshi@gmail.com>
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

import std.stdio;
import std.cstream;
import std.file;
import std.string;
import std.conv;


/* Interfaces */
interface Insn {
  Value Eval(Env);
  string toString();
}

interface Value
{
  Value opCall(Value);
  string toString();
  @property ubyte charValue();
}

/* Environment Class */
class Env
{
  private Env _next;
  private Value _v;

  this() {
    _v = null;
    _next = null;
  }
  
  this(Value v, Env e) {
    _v = v;
    _next = e;
  }
  
  @property bool empty() { return _v is null; }
  @property private Env next() { return _next; }

  Env push(Value v) {
    return new Env(v, this);
  }

  Value opIndex(int i) {
    if (this.empty) {
      throw new Exception("Env: range error");
    } else if (i == 0) {
      return _v;
    } else {
      return _next[i - 1];
    }
  }
  
  override {
    string toString () {
      if (this.empty) return "Env[]";
      
      string buf = "Env[" ~ _v.toString();
      auto d = _next;
      while (!d.empty) {
        buf ~= ", " ~ d[0].toString();
        d = d.next;
      }
      return buf ~ "]";
    }
  }
}

Env makeEnv(Value[] vs...) {
  auto e = new Env();
  foreach (v; vs) {
    e = e.push(v);
  }
  return e;
}

Value evalCode(Insn[] code, Env e) {
  auto d = e;
  foreach (i; code) {
    d = d.push(i.Eval(d));
  }
  return d[0];
}


/* Instruction Classes */
class App : Insn
{
  int _fun;
  int _arg;

  this (int fun, int arg) {
    _fun = fun;
    _arg = arg;
  }
  
  Value Eval(Env d) {
    return d[_fun](d[_arg]);
  }

  override {
    string toString() {
      return format("App[%d, %d]", _fun, _arg);
    }
  }
}

class Abs : Insn
{
  Insn[] _code;
  this(Insn[] code) {
    _code = code;
  }
  
  Value Eval(Env d) {
    return cast(Value)new Fn(_code, d);
  }

  override {
    string toString() {
      return "Abs" ~ _code.to!string();
    }
  }
}


/* Value classes */
class Fn : Value
{
  private Env _dump;
  private Insn[] _code;

  this(Insn[] code, Env e) {
    _dump = e;
    _code = code;
  }

  override {
    string toString () {
      return "Fn" ~ _code.to!string();
    }
  }
  
  Value opCall(Value v) {
    return evalCode(_code, _dump.push(v));
  }

  @property ubyte charValue() { throw new Exception("not a CharFn, but Fn"); }
}


class Builtin : Value
{
  private string _name;
  private Value delegate (Value) _dg;

  this(string name, Value delegate (Value) dg) {
    _name = name;
    _dg = dg;
  }

  override {
    string toString() {
      return "<Builtin: " ~ _name ~ ">";
    }
  }
  
  Value opCall(Value v) { return _dg(v); }

  @property ubyte charValue() { throw new Exception("not a CharFn, but Built-in Function"); }
}


Builtin ctrue, cfalse, succ;


class CharFn : Value
{
  private ubyte _char;

  this(ubyte n) {
    _char = n;
  }

  override {
    string toString() {
      return format("<CharFn: %d>", _char);
    }
  }
  
  Value opCall(Value v) {
    try {
      return (_char == v.charValue) ? ctrue : cfalse;
    } catch (Exception e) {
      return cfalse;
    }
  }

  @property ubyte charValue() {
    return _char;
  }
}

Builtin makeIn(InputStream rr) {
  return new Builtin("In", delegate Value (Value eof) {
    if (rr.eof) {
      return eof;
    } else {
      ubyte buf;
      rr.read(buf);
      return new CharFn(buf);
    }
  });
}

Builtin makeOut(OutputStream wr) {
  return new Builtin("Out", delegate Value (Value v) {
    wr.write(cast(char)v.charValue);
    return v;
  });
}

void initBuiltins() {
  ctrue = new Builtin("church true", delegate Value (Value x) {
    return new Builtin("const x", delegate Value (Value y) {
      return x;
    });
  });

  cfalse = new Builtin("church false", delegate Value (Value x) {
    return new Builtin("id", delegate Value (Value y) {
      return y;
    });
  });

  succ = new Builtin("Succ", delegate Value (Value v) {
    return new CharFn(v.charValue + 1 & 0xff);
  });
}


/* Parser */
Insn[] parseApps(string s) {
  Insn[] apps;
  while (s.length > 0) {
    int f = s.indexOf('w');
    if (f < 0) throw new Exception("syntax error");
    s = s[f .. s.length];
    int a = s.indexOf('W');
    if (a < 0) a = s.length;
    apps ~= new App(f - 1, a - 1);
    s = s[a .. s.length];
  }
  return apps;
}

Insn parseAbs(string s) {
  int arity = s.indexOf('W');
  if (arity < 0) arity = s.length;
  auto a = new Abs(parseApps(s[arity .. s.length]));
  for (int i = 1; i < arity; i++) {
    auto c = new Insn[1];
    c[0] = a;
    a = new Abs(c);
  }
  return a;
}

Insn[] parseProg(string src) {
  src = removechars(tr(src, "\uff37\uff57\uff36\uff56V", "Wwvvv"), "^wWv");
  auto start = src.indexOf('w');
  if (start < 0) {
    throw new Exception("error : program must have at least one function");
  }

  Insn[] code;
  foreach (c; split(src[start .. src.length], "v")) {
    if (c.length == 0) continue;
    
    if (c[0] == 'w') {
      code ~= parseAbs(c);
    } else {
      code ~= parseApps(c);
    }
  }
  return code ~ new App(0, 0);
}



int main (string[] args) {
  if (args.length < 2) {
    derr.writefln("usage: grass.exe source.grass");
    return 1;
  }
  
  initBuiltins();

  auto reader = makeIn(din);
  auto writer = makeOut(dout);
  auto initialEnv = makeEnv(reader, new CharFn('w'), succ, writer);

  Insn[] code;
  try {
    auto source = readText(args[1]);
    code = parseProg(source);
  }
  catch (Exception e) {
    derr.writefln(e.msg);
    return 2;
  }

  try {
    evalCode(code, initialEnv);
  } catch (Exception e) {
    derr.writefln(e.msg);
    return 3;
  }
  
  return 0;
}


unittest {
  // utilities
  Value idfn (string disp) {
    return new class (disp) Value {
      string _disp;
      this(string disp) { _disp = disp; }
      Value opCall(Value v) { return v; }
      @property ubyte charValue() { return 0; }
      override { string toString() { return _disp; } }
    };
  }
  
  Insn cpi (int n) {
    return new class (n) Insn {
      int _n;
      this (int n) { _n = n; }
      Value Eval(Env e) { return e[_n]; }
      override { string toString() { return null; }}
    };
  }

  /* Env */
  auto id1 = idfn("id1");
  auto e = makeEnv(id1);
  auto d = e.push(idfn("id2")).push(idfn("id3"));

  assert(e[0] == id1);
  assert(d[0].toString == "id3");
  assert(d[1].toString == "id2");
  assert(d[2].toString == "id1");
  
  assert(evalCode([cpi(0)], d).toString() == "id3");
  assert(evalCode([cpi(1)], d).toString() == "id2");
  assert(evalCode([cpi(2)], d).toString() == "id1");
  assert(evalCode([cpi(2), cpi(0), cpi(0)], d).toString() == "id1");
  
  /* CharFn and Succ */
  initBuiltins();
  Value a, b, x, y;

  a = new CharFn('a');
  b = succ(a);
  x = new CharFn('x');
  y = succ(x);
  
  assert(x.charValue == 120);
  assert(y.charValue == 121);
  assert(succ(new CharFn(255)).charValue == 0);

  // if a == a then x else y
  assert(a(a)(x)(y).charValue == x.charValue);

  // if a == b then x else y
  assert(a(b)(x)(y).charValue == y.charValue);

  /* Abs, App, Fn */
  auto c2 = (new Abs([new Abs([new App(1, 0), new App(2, 0)])])).Eval(e);
  assert(c2.toString() == "Fn[Abs[App[1, 0], App[2, 0]]]");
  assert(c2(c2)(c2)(succ)(new CharFn(0)).charValue == 16); // 2^(2^2)
  
  /* IO */
  char[100] buf;
  auto mi = new TArrayStream!(char[]) (cast(char[])"test");
  auto mo = new TArrayStream!(char[]) (buf);
  Value rr = makeIn(mi);
  Value wr = makeOut(mo);
  for (int i = 0; i < 5; i++) {
    wr(rr(x));
  }
  mo.seekSet(0);
  assert(mo.readString(5) == "testx");

  /* parser */
  assert(parseApps("Www").to!string() == "[App[0, 1]]");
  assert(parseApps("WWwWWWw").to!string() == "[App[1, 0], App[2, 0]]");
  assert(parseAbs("w").to!string() == "Abs[]");
  assert(parseAbs("wwWWwWWWw").to!string() == "Abs[Abs[App[1, 0], App[2, 0]]]");
  assert(parseProg("wWWwwww").to!string() == "[Abs[App[1, 3]], App[0, 0]]");

}
