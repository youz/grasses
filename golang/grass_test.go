package main

import (
	"bytes"
	"fmt"
	"strings"
	"testing"
)


func TestEnv(t *testing.T) {
	s := &Succ
	e0 := &Env{CharFn('a')}
	e := e0
	for i := 1; i < 10; i++ {
		e = e.cons(s.Apply(e.nth(0)))
	}
	if len(*e0) != 1 {
		t.Errorf("len(*e0) got %d; should be 1", len(*e0))
	}
	if len(*e) != 10 {
		t.Errorf("len(*e) got %d; should be 10", len(*e))
	}
	for i := 0; i < len(*e); i++ {
		a, _ := e.nth(i).GetValue()
		b, _ := (*e)[len(*e)-i-1].GetValue()
		if a != b {
			t.Errorf("e.nth(%d).GetValue() got %d; should be %d", i, a, b)
		}
	}
}

func TestPrimitives(t *testing.T) {
	a := CharFn('A')
	b := Succ.Apply(a)

	ctrue := a.Apply(a)
	v := ctrue.Apply(a).Apply(b)
	if v != a {
		t.Errorf("expected %s; got %s", a, v)
	}

	cfalse := a.Apply(b)
	v = cfalse.Apply(a).Apply(b)
	if v != b {
		t.Errorf("expected %s; got %s", b, v)
	}

	w := bytes.NewBuffer([]byte{})
	out := NewOut(w)
	out.Apply(a)
	out.Apply(b)
	if w.String() != "AB" {
		t.Errorf("expected `AB`; got `%s`", w.String())
	}
}

func TestEvalCode(t *testing.T) {
	c := Code{&App{fun: 2, arg: 1}, &App{fun: 0, arg:1}}
	e := Env{ChurchTrue, CharFn(64), CharFn(65)}
	v, _ := EvalCode(&c, &e).GetValue()
	if v != 64 {
		t.Errorf("expected 64; got %d", v)
	}
}

func TestParse(t *testing.T) {
	src := []byte("wｖＷＷｗｗｗwhogeWWWwwwwwfuga")
	parsed, needapp := ParseGrass(src)
	dmp := fmt.Sprintf("%s", parsed)
	ans := "[Abs[] App(1, 3) App(2, 4)]"
	if needapp || dmp != ans {
		t.Errorf("expected `%s`, false;\n got `%s`, %s", ans, dmp, needapp)
	}
}

type GrassTest struct {
	title string
	code string
	input string
	output string
}

var grassTests = []GrassTest {
	{"w", "wWWwwww", "", "w"},
	{"v", "wwWWWWwWWWWWWwWwwwWwwwwwWwWwwwwwWWWWwvWwWwwwwwWWWWw", "", "v"},
	{"succ&out", "wWWwWWWWwvwwWWwWWWwvWwWwwwWwwwwwww", "", "wxyz"},
	{"echo", "wwwvwWWWWWWwwWwWwwwwwWwWwwwwWWWWwwwwwwWwWw", "asdfqwer", "asdfqwer"},
	{"helloworld", "wwWWWWwWwwwWwwwWwwwvwwWWwWWWwvWwWwvwWWWWWwWWWwWwwwvwvWwwwwwWwwwWWwWWWwWWWWWWWWwWWWWWWwwwwwwwwwwwwwwWWWWWWWWwWWWWWwWWWWWwwwWWWWWWwwwWWWWWWWWWwWWWWWWWWWWwwwWWWWWWWWWwWWWWWWWWWWwwwWWWWWWWWWWwwwwWWWWWWWWWWWWWwWWWWWWWWWWWWwwWWWWWWWWWWWWWWWwWWWWWWWWWWWWWWWwWWWWWWWWWWWWWWWWWWWWWWWWWWWwvwWWWWWWWWWWWWWWWWWWWWWWWWWWWWwvWwwwwwwWWwwwwwwwwwwWWWwwwwwwwwwwwwwWWWWwWWWWWwwwwwwwwwwwwwwwwwWWWWWWwwwwwwwwwwWWWWWWWwwwwwwwwwvwWWWWWWWWWwvWwwwwwwwwwwwwwwwwwwwwwwwwWWwwwwwwwwwwwwwwwwwwwwwwWWWwwwwwwwwwwwwwwwwwwwwwwwwWWWWwwwwwwwwWWWWWwwwwwwwwwwwwwwwwwwwwWWWWWWwwwwwwwwwwwwwww",
		"", "Hello, world!"},
}

func TestGrass(t *testing.T) {
	for _, test := range grassTests {
		w := bytes.NewBuffer([]byte{})
		r := strings.NewReader(test.input)
		RunGrass([]byte(test.code), r, w)
		output := w.String()
		fmt.Println(output)
		if output != test.output {
			t.Errorf("%s: expected `%s`; got `%s`", test.title, test.output, output)
		}
	}
}
