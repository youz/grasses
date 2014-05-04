/*
 * grassctc.d - Compile Time Grass to D Compiler
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
import std.string;
import std.conv;


/* grass to D compiler */
string compile(string src) {
  int symn = 0;
  string gensym () {
    return text("f", symn++);
  }
  string space(int n) {
    return std.array.replicate(" ", n);
  }
  
  interface Insn {
    string toD(string[], int);
  }
  
  string code2d (Insn[] code, string[] env, int indent) {
    if (code.length == 0) {
      return env[0];
    } else {
      auto buf = code[0].toD(env, indent);
      if (code.length > 1) {
        auto arg = gensym();
        return "(new F(delegate V(V " ~ arg ~ ") {\n"
             ~ space((indent + 1) * 2) ~ "return "
             ~ code2d(code[1 .. code.length], [arg] ~ env, indent + 1) ~ ";\n"
             ~ space(indent * 2) ~ "}))(" ~ buf ~ ")";
      } else {
        return buf;
      }
    }
  }
  
  class Abs : Insn {
    Insn[] _code;
    this(Insn[] code) { _code = code; }

    string toD(string[] env, int indent) {
      auto arg = gensym();
      return "new F(delegate V(V " ~ arg ~ ") {\n"
        ~ space((indent + 1) * 2) ~ "return "
        ~ code2d(_code, [arg] ~ env, indent + 1) ~ ";\n"
        ~ space(indent * 2) ~ "})";
    }
  }

  class App : Insn {
    int _m, _n;
    this(int m, int n) { _m = m; _n = n; }
    string toD(string[] env, int indent) {
      return env[_m] ~ "(" ~ env[_n] ~ ")";
    }
  }


  /* parser */
  // std.string.indexOf cannot be interpreted at compile time.
  int pos(string s, char c) {
    for (int i = 0; i < s.length; i++) {
      if (s[i] == c) {
        return i;
      }
    }
    return -1;
  }
  
  Insn[] parseApps(string s) {
    Insn[] apps;
    while (s.length > 0) {
      int f = pos(s, 'w');
      if (f < 0) throw new Exception("syntax error");
      s = s[f .. s.length];
      int a = pos(s, 'W');
      if (a < 0) a = s.length;
      apps ~= new App(f - 1, a - 1);
      s = s[a .. s.length];
    }
    return apps;
  }

  Insn parseAbs(string s) {
    int arity = pos(s, 'W');
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
    auto start = pos(src, 'w');
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

  return code2d(parseProg(src), ["writer", "succ", "w", "reader"], 0) ~ ";\n";
}

template CompileGrass(string src) {
  const char[] CompileGrass = compile(src);
  debug pragma(msg, CompileGrass);
}

/* runtime */
interface V {
  V opCall(V);
  string toString();
  @property ubyte charValue();
}

class F : V {
  private V delegate (V) _dg;
  this(V delegate (V) dg) { _dg = dg; }
  V opCall(V v) { return _dg(v); }
  @property ubyte charValue() { throw new Exception("not a CharFn"); }
  override { string toString() { return "<F>"; } }
}

F ctrue, cfalse, succ;

class CharFn : V {
  private ubyte _char;
  this(ubyte n) { _char = n; }
  override { string toString() { return format("<CharFn: %d>", _char); } }
  
  V opCall(V v) {
    try {
      return (_char == v.charValue) ? ctrue : cfalse;
    } catch (Exception e) {
      return cfalse;
    }
  }

  @property ubyte charValue() { return _char; }
}

F makeIn(InputStream rr) {
  return new F(delegate V (V eof) {
    if (rr.eof) {
      return eof;
    } else {
      ubyte buf;
      rr.read(buf);
      return new CharFn(buf);
    }
  });
}

F makeOut(OutputStream wr) {
  return new F(delegate V (V v) {
    wr.write(cast(char)v.charValue);
    return v;
  });
}


int main (string[] args) {
  auto reader = makeIn(din);
  auto writer = makeOut(dout);
  auto w = new CharFn('w');

  succ = new F(delegate V (V v) {
    return new CharFn(v.charValue + 1 & 0xff);
  });
  
  ctrue = new F(delegate V (V x) {
    return new F(delegate V (V y) {
      return x;
    });
  });

  cfalse = new F(delegate V (V x) {
    return new F(delegate V (V y) {
      return y;
    });
  });
  
  mixin(CompileGrass!(import("source.grass")));

  return 0;
}
