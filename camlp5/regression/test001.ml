(*
 * Test001: simplest ocamlyard test.
 * Copyright (C) 2006
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

class lexer (s : char list) =
  object (self : 'self) inherit stream s as super

    val ws    = regexp "[' ''\n''\t']+"
    val ident = regexp "[a-zA-Z]\([a-zA-Z0-9]\)*"
(*
    method getIDENT : 'a . (string -> 'self -> ('a, 'self) result) -> ('a, 'self) result =
      fun k ->
        let str = of_chars s in
        let p =
	  if string_match ws str 0
	  then (String.length (matched_string str))
	  else 0
        in
        if string_match ident str p
        then
	  let m = matched_string str in
	  k m (*{< p = p + 1(*String.length m*) >}*) self
        else
	  emptyResult*)

    method look : 'b . char -> (char -> 'self -> ('b, 'self) result) -> ('b, 'self) result =
      fun c k -> super # look c k
      (*
        let str = of_chars s in
        let p =
	  if string_match ws str 0
	  then (String.length (matched_string str))
	  else 0
        in
        if string_match (regexp (quote @@ of_chars @@ c :: [])) str p
        then
	  let m = matched_string str in
	  k c (*{< p = p + 1(*String.length m*) >}*)self
        else
	  let err1 = Errors.Delete (List.nth s p, p) in
	  let err2 = Errors.Replace (c, p) in
	  let res1 = (match ((*{< p = p + 1; errors = Errors.addError err1 errors >}*)self # look c k) with
		      | Parsed (res, _) -> Failed (Some err1)
		      | Failed x        -> Failed x) in
	  let res2 = (match (k c (*{< p = p + 1; errors =  Errors.addError err2 errors>}*)self) with
		      | Parsed (res, _) -> Failed (Some err2)
		      | Failed x        -> Failed x) in
	  res1 <@> res2*)

    method getEOF : 'a . (unit -> 'self -> ('a, 'self) result) -> ('a, 'self) result =
      fun k -> super # getEOF k
      (*
        let str = of_chars s in
        let p =
	  if string_match ws str 0
	  then (String.length (matched_string str))
	  else 0
        in
        if p = String.length str
        then k () self
        else emptyResult*)

    method getIDENT : 'b . (unit -> 'self -> ('b, 'self) result) -> ('b, 'self) result =
      fun k -> super # getEOF k(*
        if p = List.length s
        then k () self
        else emptyResult*)
  end

let id = ostap (IDENT)
let _ =
  begin match id (new lexer (of_string "   hasToBeParsed ")) (fun res s -> Parsed ((1, s), None)) with
  | Parsed ((str, _), _) -> Printf.printf "Parsed: \n"
  | _ -> Printf.printf "Failed.\n"
  end;
  begin match id (new lexer (of_string "   123 ")) (fun res s -> Parsed ((1, s), None)) with
  | Parsed ((str, _), _) -> Printf.printf "Parsed: \n"
  | _ -> Printf.printf "Failed.\n"
  end;
