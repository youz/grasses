(*
 * grass.sml - Grass interpreter (for SML#)
 * https://github.com/youz/grasses
 *
 * Copyright (C) 2021 Yousuke Ushiki  <citrus.yubeshi@gmail.com>
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
 *)

(* types *)
datatype insn =
    App of int * int
  | Abs of int * insn list;;

datatype value =
    Ch of int
  | Prim of (value -> value)
  | Fn of int * insn list * value list;;

(* primitives *)
fun succ(Ch c) = Ch ((c + 1) mod 256)
  | succ(x) = raise Fail "(Succ) not a char";;

fun output x =
  case x of
    Ch c => (print (Char.toString (chr c)); x)
  | _ => raise Fail "(Out) not a char";;

fun input x =
  case TextIO.input1 TextIO.stdIn of
    SOME c => Ch (ord c)
  | NONE => x;;

val ctrue = Prim(fn x => Prim(fn y => x));;
val cfalse = Prim(fn x => Prim(fn y => y));;

(* evaluator *)
fun eval code env dump =
  case (code, env) of
    ([], v :: _) =>
      (case dump of
        [] => v
       | (c', e') :: d' => eval c' (v :: e') d')
  | (i :: r, _) =>
    let
      fun next v = eval r (v :: env) dump
    in
      (case i of
         Abs(a, c) => next (Fn(a, c, env))
       | App(m, n) =>
         let
           val a = List.nth (env, (n - 1))
         in
           case List.nth (env, (m - 1)) of
             Prim f => next (f a)
           | Ch x =>
             (case a of
               Ch y => next (if x = y then ctrue else cfalse)
               | _  => next cfalse)
           | Fn(n, c', e') =>
             if n = 1 then
               (case r of
                  [] => eval c' (a :: e') dump
                | _  => eval c' (a :: e') ((r, env) :: dump))
             else
                 next (Fn(n-1, c', (a :: e' )))
         end)
    end
  | _ => raise Fail "illegal state";;

(* lexer & parser *)
fun lex s =
  let
    fun readc l =
      case l of
        #"W" :: t => (SOME(#"W"), t)
      | #"w" :: t => (SOME(#"w"), t)
      | #"v" :: t => (SOME(#"v"), t)
      | #"\239" :: t =>
        (case t of
           #"\188" :: #"\183" :: t => (SOME(#"W"), t)
         | #"\189" :: #"\151" :: t => (SOME(#"w"), t)
         | #"\189" :: #"\150" :: t => (SOME(#"v"), t)
         | _ :: _ :: t => readc t
         | _ => (NONE, [])
        )
      | _ :: t => readc t
      | [] => (NONE, [])
    fun count c n s =
      case readc s of
        (SOME(h), t) =>
          if c = h then count c (n + 1) t
          else (c, n) :: (count h 1 t)
      | (NONE, _) => [(c, n)]
  in
    List.tl (count #" " 0 (explode s))
  end;;

fun parse src =
  let
    fun find_w l =
      case l of
        ((#"w", _) :: _) => l
      | _ :: t => find_w t
      | [] => raise Fail "abs not found"
    fun split_at_v acc s =
      case s of
        (#"v", _) :: t => List.rev acc :: (split_at_v [] t)
      | h :: t => split_at_v (h :: acc) t
      | [] => [List.rev acc]
    fun parse1 l =
      case l of
        [] => []
      | (#"w", a) :: t => [Abs(a, parse1 t)]
      | (#"W", m) :: (#"w", n) :: t => App(m, n) :: (parse1 t)
      | _ => raise Fail "syntax error"
  in
    List.concat (List.map parse1 (split_at_v [] (find_w (lex src))))
  end;;

(* run *)
fun run src =
  let
    val e0 = [Prim output, Prim succ, Ch 119, Prim input]
    val d0 = [([App(1, 1)], [])]
  in
    eval (parse src) e0 d0
  end;;

fun read_all filename =
  let
    val f = TextIO.openIn filename
    val buf = TextIO.inputAll f
  in
    TextIO.closeIn f;
    buf
  end;;


(* entry point *)
let
  val argv = CommandLine.arguments ()
  fun usage () = print "Usage: grass sourcefile\n"
in
  case argv of
    [] => usage ()
  | "--help" :: _ => usage ()
  | "-h" :: _ => usage ()
  | path :: _ => (
      run (read_all (List.hd argv));
      OS.Process.exit OS.Process.success
    )
    handle
    Fail(msg) => (
      print ("Error: " ^ msg ^ "\n");
      OS.Process.exit OS.Process.failure
    )
end;;
