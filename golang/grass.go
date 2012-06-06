//
// grass.go - Grass interpreter
// https://github.com/youz/grasses
//
// Copyright (C) 2012 Yousuke Ushiki  <citrus.yubeshi@gmail.com>
// All rights reserved.
//
// Grass language
// http://www.blue.sky.or.jp/grass/
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in
//    the documentation and/or other materials provided with the
//    distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
// THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
// PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS
// BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
// BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
// OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
// IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//

package main

import (
	"bytes"
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"regexp"
)

func die(fmtstr string, args ...interface{}) {
	fmt.Fprintf(os.Stderr, fmtstr, args...)
	os.Stderr.WriteString("\n")
	os.Exit(1)
}

// Grass Value interafce
type Value interface {
	Apply(Value) Value
	GetChar() (byte, bool)
}

// Environment Type
type Env struct {
	first Value
	next *Env
}

func NewEnv(v Value, more ...Value) *Env {
	e := &Env{v, nil}
	for _, x := range more {
		e = &Env{x, e}
	}
	return e
}

func (e *Env) cons(v Value) *Env {
	return &Env{v, e}
}

func (e *Env) nth(n int) Value {
	for i := 0; i < n; i++ {
		e = e.next
		if e == nil {
			return nil
		}
	}
	return e.first
}

// Instruction Types
type Insn interface {
	Eval(*Env) Value
	String() string
}

type Code []Insn

// Abstraction
type Abs struct {
	body Code
}

func (abs *Abs) Eval(e *Env) Value {
	return &Fn{body: &abs.body, env: e}
}

func (abs *Abs) String() string {
	return fmt.Sprintf("Abs%s", abs.body)
}

// Application
type App struct {
	fun int
	arg int
}

func (app *App) Eval(e *Env) Value {
	f := e.nth(app.fun)
	if f == nil {
		die(fmt.Sprintf("App(%d, %d): %d is out of range", app.fun, app.arg, app.fun))
	}
	a := e.nth(app.arg)
	if a == nil {
		die(fmt.Sprintf("App(%d, %d): %d is out of range", app.fun, app.arg, app.arg))
	}
	return f.Apply(a)
}

func (app *App) String() string {
	return fmt.Sprintf("App(%d, %d)", app.fun, app.arg)
}

// Value Types

// Fn (Abstraction with Environment)
type Fn struct {
	body *Code
	env  *Env
}

func (fn *Fn) GetChar() (byte, bool) {
	return 0, false
}

func (fn *Fn) Apply(v Value) Value {
	return EvalCode(fn.body, fn.env.cons(v))
}

func EvalCode(c *Code, e0 *Env) Value {
	e := e0
	for _, insn := range *c {
		e = e.cons(insn.Eval(e))
	}
	return e.nth(0)
}

// Char
type CharFn byte

func (cf CharFn) GetChar() (byte, bool) {
	return byte(cf), true
}

func (cf CharFn) Apply(arg Value) Value {
	v, ok := arg.GetChar()
	if ok && v == byte(cf) {
		return ChurchTrue
	}
	return ChurchFalse
}

// Primitives
type Primitive func(v Value) Value

func (p Primitive) GetChar() (byte, bool) {
	return 0, false
}

func (p Primitive) Apply(v Value) Value {
	return p(v)
}

// IO
func NewIn(r io.Reader) Primitive {
	buf := []byte{0}
	return Primitive(func(v Value) Value {
		_, e := r.Read(buf)
		if e == io.EOF {
			return v
		}
		return CharFn(buf[0])
	})
}

func NewOut(w io.Writer) Primitive {
	return Primitive(func(v Value) Value {
		c, ok := v.GetChar()
		if !ok {
			die("Out Error: argument must be <CharFn>")
		}
		w.Write([]byte{c})
		return v
	})
}

var (
	Succ = Primitive(func(v Value) Value {
		c, ok := v.GetChar()
		if !ok {
			die("Succ Error: argument must be <CharFn>")
		}
		return CharFn(byte((c + 1) & 255))
	})
	ChurchTrue = Primitive(func(x Value) Value {
		return Primitive(func(y Value) Value {
			return x
		})
	})
	ChurchFalse = Primitive(func(x Value) Value {
		return Primitive(func(y Value) Value {
			return y
		})
	})
	/*
	ChurchTrue = &Fn {
		body: &Code{ &Abs{ body: &Code{ &App{ fun: 3, arg: 2}}}},
		Env: &Env{ &Fn{ body: &Code{}, env: &Env{}}},
	}
	ChurchFalse = &Fn {body: &Code{}, env: &Env{}}
	*/
)

func ParseGrass(src []byte) (Code, bool) {
	filter := func(r rune) rune {
		switch r {
		case 'w', 'ｗ':
			return 'w'
		case 'W', 'Ｗ':
			return 'W'
		case 'v', 'V', 'ｖ', 'Ｖ':
			return 'v'
		}
		return -1
	}
	pat := regexp.MustCompile("^(w*)(W+w+)*$")
	apppat := regexp.MustCompile("(W+)(w+)")
	insts := bytes.Split(bytes.Map(filter, src), []byte{'v'})
	tmp := make([]*Code, len(insts))
	codelen := 0
	var end_with_abs bool
	for i, p := range insts {
		if len(p) == 0 {
			die("syntax error at section %d", i)
		}
		m := pat.FindSubmatchIndex(p)
		if len(m) == 0 {
			die("syntax error at section %d", i)
		}
		arity := m[3] - m[2]
		apps := apppat.FindAllSubmatchIndex(p[arity:], -1)
		body := make(Code, len(apps))
		end_with_abs = (arity > 0)
		for j, m := range apps {
			body[j] = &App{fun: m[3] - m[2] - 1, arg: m[5] - m[4] - 1}
		}
		for arity > 0 {
			body = Code{&Abs{body: body}}
			arity--
		}
		tmp[i] = &body
		codelen += len(body)
	}
	code := make(Code, codelen, codelen+1)
	i := 0
	for _, c := range tmp {
		for _, insn := range *c {
			code[i] = insn
			i++
		}
	}
	return code, end_with_abs
}

func RunGrass(src []byte, r io.Reader, w io.Writer) {
	code, needapp := ParseGrass(src)
	if needapp {
		code = append(code, &App{fun: 0, arg: 0})
	}
	e := NewEnv(NewIn(r), CharFn(119), Succ, NewOut(w))
	EvalCode(&code, e)
}

func readSource(path string) ([]byte, error) {
	var r io.Reader
	if path == "-" {
		r = os.Stdin
	} else {
		f, err := os.Open(path)
		if err != nil {
			return nil, err
		}
		defer f.Close()
		r = f
	}
	return ioutil.ReadAll(r)
}

func usage() {
	fmt.Fprintf(os.Stderr, "usage: grass [path]\n")
	// flag.PrintDefaults()
	os.Exit(2)
}

func main() {
	flag.Usage = usage
	flag.Parse()

	var (
		src []byte
		err error
	)
	if flag.NArg() == 0 {
		src, err = readSource("-")
	} else {
		src, err = readSource(flag.Arg(0))
	}
	if err != nil {
		die("%s", err)
	}
	RunGrass(src, os.Stdin, os.Stdout)
}
