(*
 * test012: regression test.
 * Copyright (C) 2006-2008
 * Dmitri Boulytchev, St.Petersburg State University
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file COPYING).
 *)

open Re_str
open Ostap
open Types
open Result
open Errors
open Matcher
open Printf

class lexer (s : char list) =
  let ident = Re_str.regexp "[a-zA-Z][a-zA-Z0-9]*" in
  let const = Re_str.regexp "[0-9]+" in
  object (self : 'self) inherit stream s as super

    method getCONST : 'b . (string -> 'self -> ('b, 'self) result) -> ('b, 'self) result =
      fun k ->
        let str = of_chars s in
	if string_match const str p
	then
          let m = matched_string str in
          k m {< p = p + String.length m >}
	else
          emptyResult

    method getIDENT : 'b . (string -> 'self -> ('b, 'self) result) -> ('b, 'self) result =
      fun k ->
        let str = of_chars s in
        if string_match ident str p
        then
	  let m = matched_string str in
	  k m {< p = p + String.length m >}
        else
	  emptyResult
  end

ostap (

  listBy[sep][item]: hd:item tl:(-sep item)* {hd :: tl};
  list: listBy[ostap (",")];

  expr [nlevels][operator][primary][level]:
    {nlevels = level} => p:primary {`Primary p}
  | {nlevels > level} => left:expr[nlevels][operator][primary][level+1]
       right:(
          operator[level]
          expr[nlevels][operator][primary][level](*::("operand expected")*)
       )?
       {
        match right with
	| None -> left
	| Some (op, right) -> `Operator (left, op, right)
       }

)

ostap (
  primary:
    i:IDENT             {`Ident i}
  | c:CONST             {`Const c}
  | -"(" intExpr -")"
  | "-" p:primary       {`Neg p};

  operator[n]:
     {n == 0} => ("+" {`Plus} | "-" {`Minus})
   | {n == 1} => ("*" {`Mul } | "/" {`Div  })
  ;
  intExpr: p:expr[2][operator][primary][0];
  main: intExpr -EOF
)

let _ =
  match main (new lexer (of_string "a+b-")) (fun res s -> (*match res with
	                                                   | `I _ ->*) Parsed ((1, s), None)) with
  | Parsed _ -> Printf.printf "Parsed.\n"
  | Failed _ -> Printf.printf "Not parsed."
