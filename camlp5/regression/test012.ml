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
  main: i:IDENT {`I i}
)

let _ =
  match main (new lexer (of_string "a+b+c")) (fun res s -> Parsed ((res, s), None)) with
  | Parsed _ -> Printf.printf "Parsed.\n"
  | Failed _ -> Printf.printf "Not parsed."
