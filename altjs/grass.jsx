/*
 * grass.jsx - Grass interpreter
 * https://github.com/youz/grasses
 *
 * Copyright (C) 2012 Yousuke Ushiki  <citrus.yubeshi@gmail.com>
 * All rights reserved.
 *
 * Grass language
 * http://www.blue.sky.or.jp/grass/
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *	  notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *	  notice, this list of conditions and the following disclaimer in
 *	  the documentation and/or other materials provided with the
 *	  distribution.
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

import "test-case.jsx";

interface Value {
	function getChar(): int;
	function apply(v: Value): Value;
}

interface Insn {
	function eval(e: Value[]): Value;
}

class Abs implements Insn {
	var _code: Insn[];

	function constructor(code: Insn[]) {
		this._code = code;
	}
	
	override function eval(env: Value[]): Value {
		return new Fn(env, this._code);
	}
	override function toString(): string {
		return "Abs[" + this._code.join(",") + "]";
	}
}

class App implements Insn {
	var _m: int;
	var _n: int;

	function constructor(m: int, n: int) {
		this._m = m;
		this._n = n;
	}
	
	override function eval(env: Value[]): Value {
		return env[this._m].apply(env[this._n]);
	}
	override function toString(): string {
		return "App(" + this._m as string + "," + this._n as string + ")";
	}
}

class Fn implements Value {
	var _env: Value[];
	var _code: Insn[];

	function constructor(env: Value[], code: Insn[]) {
		this._env = env;
		this._code = code;
	}

	override function getChar(): int {
		return -1;
	}
	override function apply(v: Value): Value {
		return Grass.evalCode(this._code, [v].concat(this._env));
	}
}

class CharFn implements Value {
	var _char: int;
	
	function constructor(v: int) {
		this._char = v;
	}

	override function getChar(): int {
		return this._char;
	}
	override function apply(v: Value): Value {
		var c = v.getChar();
		return c == this._char ? Grass.CTrue: Grass.CFalse;
	}
}

class Primitive implements Value {
	var _fn: (Value)->Value;
	
	function constructor(f: (Value)->Value) {
		this._fn = f;
	}
	
	override function getChar(): int {
		return -1;
	}
	override function apply(v: Value): Value {
		return this._fn(v);
	}
}

class Grass {
	static var Succ = new Primitive((x) -> {
		var c = x.getChar();
		if (c < 0) {
			throw "Succ Error: argument should be <CharFn>";
		}
		return new CharFn((c + 1) & 255);
	});
	static var CTrue = new Primitive((x) -> new Primitive((y) -> x));
	static var CFalse = new Primitive((x) -> new Primitive((y) -> y));
	
	static function makeOut(outfn: (int)->void): Primitive {
		return new Primitive((v) -> {
			var c = v.getChar();
			if (c < 0) {
				throw "Out Error: argument should be <CharFn>";
			}
			outfn(c);
			return v;
		});
	}
	
	static function makeIn(infn: ()->int): Primitive {
		return new Primitive((v) -> {
			var c = infn();
			if (c < 0) {
				return v;
			} else {
				return new CharFn(c);
			}
		});
	}
	
	static function parse(src: string): Insn[] {
		src = src.replace("\uFF37", "W").replace("\uFF57", "w").
		  replace("\uFF56", "v").replace("\uFF36", "v").replace(/^[^w]+/, "");
		var code = []: Insn[];
		var p = src.split(/v/);
		for (var i in p) {
			var s = p[i].match(/W*w+/g);
			var arity = (s[0].charAt(0) == "w") ? s.shift().length: 0;
			var c = []: Insn[];
			for (var j in s) {
				var _ = s[j].match(/^(W+)(w+)$/);
				c.push(new App(_[1].length-1, _[2].length-1));
			}
			while (arity-- > 0) {
				c = [new Abs(c) as Insn];
			}
			code = code.concat(c);
		}
		return code;
	}

	static function evalCode(code: Insn[], env: Value[]): Value {
		for (var i in code) {
			var v = code[i].eval(env);
			env = [v].concat(env);
		}
		return env[0];
	}

	static function run(src:string, stdout: (int)->void, stdin: ()->int): void {
		var code = Grass.parse(src);
		if ((code[code.length-1] as Insn) instanceof Abs) {
			code.push(new App(0, 0));
		}
		var env = [Grass.makeOut(stdout), Grass.Succ, new CharFn(119), Grass.makeIn(stdin)]: Value[];
		Grass.evalCode(code, env);
	}
}


class _Test extends TestCase {
	static var src = "wwWWwWWWwvWwWwWwwwwWwwwwwww";

	function testPrimitive(): void {
		var c = new CharFn(65);
		this.expect(Grass.CFalse.apply(c).apply(Grass.Succ.apply(c)).getChar()).toBe(66);
	}
	
	function testParse(): void {
		this.expect(Grass.parse(_Test.src).toString()).
		  toBe("Abs[Abs[App(1,0),App(2,0)]],App(0,0),App(0,0),App(0,3),App(0,6)");
	}
	
	function testRun(): void {
		var output = "";
		Grass.run(_Test.src,
				  (i) -> { output += String.fromCharCode(i); },
				  () -> -1);
		this.expect(output.match(/^w{256}$/)[0]).toBe(output);
	}

	function testIO(): void {
		var output = "";
		var input = "asdfzxcv";
		Grass.run("wwwvwWWWWWWwwWwWwwwwwWwWwwwwWWWWwwwwwwWwWw",
				  (i) -> { output += String.fromCharCode(i); },
				  () -> {
					  if (input == "") {
						  return -1;
					  } else {
						  var c = input.charCodeAt(0);
						  input = input.slice(1);
						  return c;
					  }
				  });
		this.expect(output).toBe("asdfzxcv");
	}
}
