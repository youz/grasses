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

open System
open System.IO
open System.Text
open System.Text.RegularExpressions

type Insn =
    | App of int * int
    | Abs of Insn list

type Value =
    | Ch of byte
    | Prim of (Value -> Value)
    | Fn of Insn list * Value list

let rec i2s i =
    match i with
    | App(m, n) -> String.Format("app({0},{1})", m, n)
    | Abs c -> String.Format("abs({0})", String.Join(",", (List.map i2s c)))

let v2s =
    function
    | Ch c -> String.Format("char({0})", c)
    | Prim _ -> "primitive"
    | Fn(c, e) -> String.Format("func({0})", String.Join(",", (List.map i2s c)))

let ctrue = Prim(fun x -> Prim(fun _ -> x))
let cfalse = Prim(fun _ -> Prim(id))

let Succ =
    function
    | Ch c -> (int c + 1) % 256 |> byte |> Ch
    | x -> failwithf "Succ: %A is not a CharFn" x

let Output (buf: MemoryStream) =
    fun x ->
        match x with
        | Ch c ->
            buf.WriteByte(byte c)
            x
        | _ -> failwithf "Out: %A is not a CharFn" x

let Input(istr: Stream) =
    let cap = 0x4000
    let buf = Array.zeroCreate cap
    let mutable pos = cap
    let mutable readlen = cap
    fun (eof: Value) ->
        if readlen = 0 then
            eof
        else if pos = readlen then
            readlen <- istr.Read(buf, 0, cap)
            pos <- 1
            if readlen = 0 then
                eof
            else
                Ch buf.[0]
        else
            let ret = Ch buf.[pos]
            pos <- pos + 1
            ret


let rec Eval c e =
    match (c, e) with
    | ([], v :: r) -> v
    | (i :: r, e) ->
        match i with
        | Abs c -> Eval r (Fn(c, e) :: e)
        | App(m, n) ->
            let a = e.[n - 1]
            match e.[m - 1] with
            | Prim f -> Eval r (f a :: e)
            | Ch x ->
                match a with
                | Ch y when x = y -> Eval r (ctrue :: e)
                | _               -> Eval r (cfalse :: e)
            | Fn(c', e') ->
                match r with
                | [] -> Eval c' (a :: e')
                | r  -> Eval r ((Eval c' (a :: e')) :: e)
    | _ -> failwithf "illegal state"


let Parse (src : string) =
    let rec makeabs n body =
        if n = 1 then Abs(body)
        else makeabs (n - 1) [Abs(body)]
    let rec parseapps xs =
        match xs with
        | "" -> []
        | s  ->
            let l = String.length s
            let m = s.IndexOf 'w'
            let n = s.IndexOf('W', m)
            if n = -1 then [App(m, l - m)]
            else App(m, n - m) :: parseapps (s.Substring n)
    let parse1 (s : string) =
        match (s.IndexOf 'W') with
        | -1 -> [makeabs (String.length s) []]
        | 0  -> parseapps s
        | a  -> [makeabs a (parseapps (s.Substring a))]
    let filter c =
        match c with
        | 'w' | 'ｗ' -> "w"
        | 'W' | 'Ｗ' -> "W"
        | 'v' | 'ｖ' -> "v"
        | _ -> ""    
    String.collect filter src
        |> fun s -> Regex.Replace(s, "^[^w]+", "")
        |> fun s -> Regex.Split(s, "v")
        |> List.ofArray
        |> List.filter (fun s -> s <> "")
        |> List.fold (fun acc s -> List.append acc (parse1 s)) []

let Run src =
    let ostr = new MemoryStream ()
    let istr = Console.OpenStandardInput ()
    let initEnv = [ Prim (Output ostr); Prim Succ; Ch (byte 119); Prim (Input istr)]
    let code = List.append (Parse src) [App(1, 1)]
    Eval code initEnv |> ignore
    let wr = Console.OpenStandardOutput ()
    wr.Write (ostr.ToArray(), 0, int ostr.Length)


[<EntryPoint>]
let main args =
  if args.Length = 0 then
    printfn "usage: grass.exe sourcefile"; 1
  else
    try
      File.ReadAllText args.[0] |> Run; 0
    with
    | ex -> eprintfn "Error: %s" (ex.ToString()); 1
