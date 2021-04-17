(*
 * grass.fs - Grass interpreter
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
 *)

(* types *)
type insn =
  | App of int * int
  | Abs of int * insn list;;

type value =
  | Ch of int
  | Prim of (value -> value)
  | Fn of int * insn list * value list;;

(* primitives *)
let succ = function
  | Ch c -> Ch ((c + 1) mod 256)
  | _ -> failwith "(Succ) not a char";;

let output = function
  | Ch c as x -> print_char (char_of_int c); x
  | _ -> failwith "(Out) not a char";;

let input x =
  try Ch (input_char stdin |> int_of_char)
  with End_of_file -> x;;

let ctrue = Prim(fun x -> Prim(fun y -> x));;
let cfalse = Prim(fun x -> Prim(fun y -> y));;

(* evaluator *)
let rec eval code env dump =
  match (code, env) with
  | ([], v :: _) ->
    begin match dump with
    | [] -> v
    | (c', e') :: d' -> eval c' (v :: e') d'
    end
  | (i :: r, _) ->
    let next v = eval r (v :: env) dump in
    begin match i with
    | Abs(a, c) -> next (Fn(a, c, env))
    | App(m, n) ->
      let a = List.nth env (n - 1) in
      begin match List.nth env (m - 1) with
      | Prim f -> next (f a)
      | Ch x ->
        begin match a with
        | Ch y when x = y -> next ctrue
        | _               -> next cfalse
        end
      | Fn(n, c', e') ->
        if n = 1 then
          begin match r with
          | [] -> eval c' (a :: e') dump
          | _  -> eval c' (a :: e') ((r, env) :: dump)
          end
        else
          next (Fn(n-1, c', (a :: e')))
      end
    end
  | _ -> failwith "illegal state";;

(* parser *)
let string_explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

let rec drop_until pred = function
  | [] -> []
  | h :: t as l ->
    if pred h then l
    else drop_until pred t;;

let rec split f l =
  let rec aux l acc =
    match l with
    | h :: t ->
      if f h then List.rev acc :: (aux t [])
      else aux t (h :: acc)
    | [] -> [List.rev acc]
  in
    aux l [];;

let lex s =
  let rec readc = function
    | 'W' :: t -> Some('W'), t
    | 'w' :: t -> Some('w'), t
    | 'v' :: t -> Some('v'), t
    | '\xef' :: t ->
      begin match t with
      | '\xbc' :: '\xb7' :: t -> Some('W'), t
      | '\xbd' :: '\x97' :: t -> Some('w'), t
      | '\xbd' :: '\x96' :: t -> Some('v'), t
      | _ :: _ :: t -> (readc t)
      | _ -> None, []
      end
    | _ :: t -> readc t
    | [] -> None, []
  in
  let rec count c n s = (
    match readc s with
    | Some(h), t ->
      if c = h then count c (n + 1) t
      else (c, n) :: (count h 1 t)
    | None, _ -> [(c, n)]
  ) in
  string_explode s |> count ' ' 0 |> List.tl;;

let parse src =
  let rec parse1 = function
    | [] -> []
    | ('w', a) :: t -> [Abs(a, parse1 t)]
    | ('W', m) :: ('w', n) :: t -> App(m, n) :: (parse1 t)
    | _ -> failwith "syntax error"
  in
    lex src
    |> drop_until (fun (c, _) -> c = 'w')
    |> function [] -> failwith "abs not found" | l -> l
    |> split (fun (c, _) -> c = 'v')
    |> List.map parse1 |> List.concat;;

(* run *)
let run src =
  let prog = parse src in
  let e0 = [Prim output; Prim succ; Ch 119; Prim input] in
  let d0 = [([App(1, 1)], [])] in
    eval prog e0 d0;;

let read_all filename =
  let f = open_in_bin filename in
  let size = in_channel_length f in
  let result = really_input_string f size in
  close_in f;
  result;;


(* entry point *)
if Array.length Sys.argv = 1 || Sys.argv.(1) = "--help" || Sys.argv.(1) = "-h" then (
  print_string ("usage: " ^ Filename.basename Sys.argv.(0) ^ " <source.grass>\n");
  exit 1
);;

try
  read_all Sys.argv.(1) |> run
with Failure(msg) ->
  prerr_string ("Error: " ^ msg ^ "\n");
  exit 1;;
