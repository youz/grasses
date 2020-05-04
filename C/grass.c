/*
 * grass.c - Grass interpreter
 * https://github.com/youz/grasses
 *
 * Copyright (C) 2020 Yousuke Ushiki  <citrus.yubeshi@gmail.com>
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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <time.h>
#include <assert.h>
#include <stdint.h>
#include <unistd.h>
#include <sys/stat.h>
#include <gc.h>

#define malloc GC_malloc
#define OUTBUF_SIZE 0x4000

void error(char *format, ...)
{
  va_list arg;
  va_start(arg, format);
  vfprintf(stderr, format, arg);
  va_end(arg);
  exit(1);
}

// instruction
enum InsnType { APP, ABS };

struct Code;
typedef struct Insn {
  enum InsnType tag;
  union {
    // App
    struct {int fun; int arg;};
    // Abs
    struct {int arity; struct Code *code;};
  };
} Insn;

typedef struct Code {
  struct Insn **buf;
  int cap;
  int len;
} Code;

#define IsApp(i)	(i->tag == APP)
#define IsAbs(i)	(i->tag == ABS)

Insn* insn_buf;
int insn_buf_cap;
int insn_buf_pos;

void init_insn_buf(int cap) {
  insn_buf = malloc(cap * sizeof(Insn));
  insn_buf_cap = cap;
  insn_buf_pos = 0;
}

Insn *make_insn(enum InsnType tag) {
  if (insn_buf_pos + 1 == insn_buf_cap) {
    int newcap = insn_buf_cap * 2;
    Insn* newbuf = malloc(newcap * sizeof(Insn));
    memcpy(newbuf, insn_buf, insn_buf_cap * sizeof(Insn));
    insn_buf = newbuf;
    insn_buf_cap = newcap;
  }
  insn_buf_pos++;
  Insn *insn = insn_buf + insn_buf_pos;
  insn->tag = tag;
  return insn;
}

Insn *make_app(int m, int n) {
  assert(m > 0 && n > 0);
  Insn *app = make_insn(APP);
  app->fun = m;
  app->arg = n;
  return app;
}

Code *make_blank_code(int cap) {
  Code *c = malloc(sizeof(Code));
  c->buf = malloc(cap * sizeof(Insn*));
  c->cap = cap;
  c->len = 0;
  return c;
}

#define make_code(...) \
  _make_code( \
    (Insn*[]){ __VA_ARGS__ }, \
    sizeof((Insn*[]){ __VA_ARGS__ }) / sizeof(Insn*) \
  )

Code *_make_code(Insn **list, size_t count)
{
  Code *c = make_blank_code(count);
  c->len = count;
  for (int i = 0; i != count; ++i)
    c->buf[i] = list[i];

  return c;
}

void code_append(Code *c, Insn* insn) {
  if (c->len == c->cap) {
    int newcap = c->cap * 2;
    Insn **newbuf = malloc(newcap * sizeof(Insn*));
    memcpy(newbuf, c->buf, c->cap * sizeof(Insn*));
    // free(c->buf);
    c->buf = newbuf;
    c->cap = newcap;
  }
  c->buf[c->len] = insn;
  c->len++;
}

Insn *make_abs(int arity, Code* body) {
  assert(arity > 0);
  Insn *abs = make_insn(ABS);
  abs->arity = arity;
  if (body != NULL)
  {
    abs->code = body;
  } else {
    abs->code = make_blank_code(8);
  }
  return abs;
}


// value
enum ValueType {
  CH,
  FN,
  PRIM_SUCC,
  PRIM_OUT,
  PRIM_IN
};

typedef struct Env {
  struct Value *v;
  struct Env *next;
  int d;
} Env;

typedef struct {
  int arity;
  Code *code;
  Env *dump;
} Fn;

typedef struct Value {
  enum ValueType tag;
  union {
    char chr;
    Fn *fun;
  };
} Value;

Env *env_push(Value *v, Env *e) {
  Env *ne = malloc(sizeof(Env));
  ne->v = v;
  ne->next = e;
  return ne;
}

Value *env_get(Env *e, int n) {
  if (e == NULL) {
    return NULL;
  } else if (n == 0) {
    return e->v;
  } else {
    return env_get(e->next, n-1);
  }
}

Value *make_prim(enum ValueType tag) {
  assert(tag >= PRIM_SUCC);
  Value *v = malloc(sizeof(Value));
  v->tag = tag;
  return v;
}

Value charmap[256];

Value *make_ch(char c) {
  return charmap + (unsigned char)c;
}

Value *make_fn(int arity, Code *code, Env *d) {
  Fn *fun = malloc(sizeof(Fn));
  fun->arity = arity;
  fun->code = code;
  fun->dump = d;
  Value *v = malloc(sizeof(Value));
  v->tag = FN;
  v->fun = fun;
  return v;
}

Value *CTRUE;
Value *CFALSE;
Env *INITIAL_ENV;

static char outbuf[OUTBUF_SIZE] = {};
static size_t outbuf_pos = 0;
void writech(char c) {
  outbuf[outbuf_pos++] = c;
  if (outbuf_pos == OUTBUF_SIZE) {
    fwrite(outbuf, sizeof(char), OUTBUF_SIZE, stdout);
    outbuf_pos = 0;
  }
}

void init() {
  // setvbuf(stdout, NULL, _IONBF, 0);
  init_insn_buf(1024);
  for (int i = 0; i<256; i++)
  {
    charmap[i].tag = CH; 
    charmap[i].chr = (char)i;
  }

  Env *e = NULL;
  e = env_push(make_prim(PRIM_IN), e);
  e = env_push(make_ch('w'), e);
  e = env_push(make_prim(PRIM_SUCC), e);
  e = env_push(make_prim(PRIM_OUT), e);

  Value *id = make_fn(1, make_blank_code(0), NULL);
  CTRUE = make_fn(2, make_code(make_app(3, 2)), env_push(id, NULL));
  CFALSE = make_fn(2, make_blank_code(0), NULL);
  INITIAL_ENV = e;
}

#define debug(...) fprintf(stderr, __VA_ARGS__)

void dump_insn(Insn *a) {
  assert(a);
  if (IsAbs(a)) {
    debug("Abs(len=%d, code=[", a->code->len);
    for (size_t i = 0; i < a->code->len; i++) {
      if (i >= 1) debug(",");
      dump_insn(a->code->buf[i]);
    }
    debug("])");
  } else {
    debug("App(%d, %d)", a->fun, a->arg);
  }
}

void dump_code(Code *c, int pos) {
  for (int i = 0; i < c->len; i++) {
    debug("%c %3d : ", (i == pos ? '>' : ' '), i + 1);
    dump_insn(c->buf[i]);
    debug("\n");
  }
}

void dump_value(Value *v) {
  if (v == NULL) {
    debug("NULL");
    return;
  }
  // printf("[%p]", v);
  switch (v->tag) {
  case FN:
    debug("Fn(arity=%d, len=%d, code=[", v->fun->arity, v->fun->code->len);
    for (size_t i = 0; i < v->fun->code->len; i++)
    {
      if (i >= 1) debug(",");
      dump_insn(v->fun->code->buf[i]);
    }
    debug("])");
    break;
  case CH:
    debug("Char('%c')", v->chr);
    break;
  case PRIM_SUCC:
    debug("Prim<Succ>");
    break;
  case PRIM_OUT:
    debug("Prim<Out>");
    break;
  case PRIM_IN:
    debug("Prim<In>");
    break;
  }
}

void dump_env(Env *e, int fpos, int apos) {
  int i = 1;
  while (e != NULL) {
    debug("%c %d : ", (i == fpos ? '<' : i == apos ? '>' : ' '), i);
    dump_value(e->v);
    debug("\n");
    e = e->next;
    i++;
  }
}

void dump_and_exit (Code *c, int pos, Env *e, int d, int fpos, int apos) {
  debug("\n---- Code ----\n");
  dump_code(c, pos);
  debug("\n---- Env (C-stack-depth=%d) ----\n", d);
  dump_env(e, fpos, apos);
  exit(1);
}

Value *eval_code(Code *c, Env *e, int depth) {
start:
  for (int i = 0; i < c->len; i++) {
    Insn* insn = c->buf[i];
    if (IsApp(insn)) {
      Value* f = env_get(e, insn->fun - 1);
      Value* a = env_get(e, insn->arg - 1);
      if (f == NULL || a == NULL) {
        debug("ERROR: env stack underflow");
        dump_and_exit(c, i, e, depth, insn->fun, insn->arg);
      }
      Value* v;
      switch (f->tag) {
      case CH:
        /*
        if (a->tag != CH) {
          debug("ERROR on CharFn: argument is not a CharFn\n");
          dump_and_exit(c, i, e, depth, insn->fun, insn->arg);
        }
         */
        if (a->tag == CH && f->chr == a->chr) {
          v = CTRUE;
        } else {
          v = CFALSE;
        }
        break;
      case FN:
        if (f->fun->arity == 1)
        {
          if (i == c->len - 1) {
            // tco
            c = f->fun->code;
            e = env_push(a, f->fun->dump);
            depth++;
            goto start;
          } else {
            v = eval_code(f->fun->code, env_push(a, f->fun->dump), depth + 1);
          }
        } else {
          v = make_fn(f->fun->arity - 1, f->fun->code, env_push(a, f->fun->dump));
        }
        break;
      case PRIM_OUT:
        if (a->tag != CH) {
          debug("ERROR on Out: argument is not a CharFn\n");
          dump_and_exit(c, i, e, depth, insn->fun, insn->arg);
        };
        writech(a->chr);
        v = a;
        break;
      case PRIM_IN:
        {
          int c = getchar();
          if (c == EOF) {
            v = a;
          } else {
            v = make_ch((char)c);
          }
        }
        break;
      case PRIM_SUCC:
        if (a->tag != CH) {
          debug("ERROR on Succ: argument is not a CharFn\n");
          dump_and_exit(c, i, e, depth, insn->fun, insn->arg);
        }
        v = make_ch(a->chr + 1);
        break;
      default:
        debug("ERROR: invalid value (tag=%d)", (int)f->tag);
        dump_and_exit(c, i, e, depth, insn->fun, insn->arg);
      }
      e = env_push(v, e);
    } else { // IsAbs(code[i])
      Value *f = make_fn(insn->arity, insn->code, e);
      e = env_push(f, e);
    }
  }
  return e->v;
}

// parser
typedef struct {
  char *buf;
  int pos;
} Reader;

Reader *read_from_file(char *file) {
  FILE *fp = fopen(file, "rb");
  if (!fp) {
    error("ERROR: fopen failed (%s)", file);
  }
  struct stat st;
  fstat(fileno(fp), &st);
  char *buf = malloc(st.st_size + 1);
  size_t o = fread(buf, 1, st.st_size, fp);
  if (o != st.st_size) {
    fclose(fp);
    error("ERROR: fread failed (%s)", file);
  }
  fclose(fp);
  buf[st.st_size] = '\0';

  Reader *rr = malloc(sizeof(Reader));
  rr->buf = buf;
  rr->pos = 0;
  return rr;
}

Reader *read_from_string(char *str) {
    Reader *rr = malloc(sizeof(Reader));
    rr->buf = str;
    rr->pos = 0;
    return rr;
}


char readc(Reader *rr) {
  for (;;) {
    if (rr->buf[rr->pos] == '\0') return '\0';
    char c = rr->buf[rr->pos++];
    if (c == 'w' || c == 'W'|| c == 'v' || c == '\0') return c;
  }
}

char peekc(Reader *rr) {
  int cur = rr->pos;
  int c = readc(rr);
  rr->pos = cur;
  return c;
}

void unread(Reader *rr) {
  if (rr->pos > 0) rr->pos--;
}

Insn *read_app(Reader *rr) {
  int c;
  int fun = 0;

  while ((c = readc(rr)) == 'W') fun++;
  if (c != 'w') {
    error("ERROR: syntax error at %d", rr->pos - 1);
  }

  int arg = 1;
  while ((c = readc(rr)) == 'w') arg++;
  if (c != '\0') unread(rr);

  return make_app(fun, arg);
}

Insn *read_abs(Reader *rr) {
  int c;
  int arity = 0;
  
  while ((c = readc(rr)) == 'w') arity++;
  unread(rr);

  Insn *abs = make_abs(arity, NULL);
  while (peekc(rr) == 'W') {
    code_append(abs->code, read_app(rr));
  }
  return abs;
}

Code *read_prog(Reader *rr) {
  Code *prog = make_blank_code(1024);
  
  int c;
  while ((c = readc(rr)) != 'w')
  {
    if (c == '\0') {
      error("ERROR: invalid program");
    }
  }
  unread(rr);

  for (;;) {
    switch (peekc(rr))
    {
      case 'w':
        code_append(prog, read_abs(rr));
        break;
      case 'W':
        code_append(prog, read_app(rr));
        break;
      case 'v':
        readc(rr);
        break;
      case '\0':
        goto fin;
    }
  }
fin:
  return prog;
}

void usage() {
  debug(
    "Usage: grass [Option] <sourcefile>\n\n"
    "  -p, --parse-only    parse and dump code (do not run)\n"
    "  -d, --dump-result   run and dump the top of Env\n"
    "  -h, --help          display this help\n"
  );
  exit(1);
}

int main(int argc, char **argv) {

  char* srcfile = NULL;
  int parse_only = 0;
  int dump_result = 0;
  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "-p") == 0 || strcmp(argv[i], "--parse-only") == 0) {
      parse_only = 1;
    } else if (strcmp(argv[i], "-d") == 0 || strcmp(argv[i], "--dump-result") == 0) {
      dump_result = 1;
    } else if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
      usage();
    } else if (argv[i][0] == '-') {
      error("ERROR: unknown option (%s)\n", argv[i]);
    } else {
      srcfile = argv[i];
    }
  }
  if (srcfile == NULL) usage();
  
  init();
  Reader *r = read_from_file(srcfile);
  Code *prog = read_prog(r);
  if (parse_only) {
    debug("\n---- Code ----\n");
    dump_code(prog, -1);
    exit(0);
  }
  code_append(prog, make_app(1, 1));
  Value *result = eval_code(prog, INITIAL_ENV, 0);
  if (outbuf_pos > 0) {
    fwrite(outbuf, sizeof(char), outbuf_pos, stdout);
  }
  if (dump_result) {
    debug("\n---- Result ----\n");
    dump_value(result);
  }
  return 0;
}


