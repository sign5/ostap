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
open Matcher

class lexer (s : char list) =
  object (self : 'self) inherit stream s as super

    val ws    = regexp "[' ''\n''\t']+"
    val ident = regexp "[a-zA-Z]\([a-zA-Z0-9]\)*"

    method getIDENT : 'a . (string -> 'self -> ('a, Reason.t, 'self) result) -> ('a, Reason.t, 'self) result =
      fun k ->
        let str = of_chars s in
        let p' =
	  if string_match ws str p
	  then p + (String.length (matched_string str))
	  else p
        in
        if string_match ident str p'
        then
	  let m = matched_string str in
	  k m {< p = p' + String.length m >}
        else
	  Failed None

    method look : 'b . string -> (string -> 'self -> ('b, Reason.t, 'self) result) -> ('b, Reason.t, 'self) result =
      fun cs k -> super # look cs k

    method getEOF : 'a . (string -> 'self -> ('a, Reason.t, 'self) result) -> ('a, Reason.t, 'self) result =
      fun k ->
        let str = of_chars s in
        let p' =
	  if string_match ws str p
	  then p + (String.length (matched_string str))
	  else p
        in
        if p' = String.length str
        then k "EOF" self
        else Failed None
  end

let id = ostap (IDENT -EOF)

let _ =
  begin match id (new lexer (of_string "  hasToBeParsed  ")) (fun res s -> Parsed ((res, s), None)) with
  | Parsed ((str, _), _) -> Printf.printf "Parsed: %s\n" str
  | _ -> Printf.printf "Failed.\n"
  end;
  begin match id (new lexer (of_string "   123 ")) (fun res s -> Parsed ((res, s), None)) with
  | Parsed ((str, _), _) -> Printf.printf "Parsed: %s\n" str
  | _ -> Printf.printf "Failed.\n"
  end;
