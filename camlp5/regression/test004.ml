(*
 * Test004: simplest ocamlyard test.
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
	  k m {< p = p + String.length m >}
        else
	  emptyResult

    method look : 'b . string -> (string -> 'self -> ('b, 'self) result) -> ('b, 'self) result =
      fun cs k -> super # look cs k

    method getEOF : 'a . (string -> 'self -> ('a, 'self) result) -> ('a, 'self) result =
      fun k ->
        let str = of_chars s in
        let p =
	  if string_match ws str 0
	  then (String.length (matched_string str))
	  else 0
        in
        if p = String.length str
        then k "EOF" self
        else emptyResult
  end

let rec br = ostap (IDENT | "(" b:br ")" {Printf.sprintf "(%s)" b})
let m = ostap (br -EOF)
let _ =
  begin match m (new lexer (of_string "left ")) (fun res s -> Parsed ((res, s), None)) with
  | Parsed ((str, _), _) -> Printf.printf "Parsed: %s\n" str
  | _ -> Printf.printf "Failed.\n"
  end;
  begin match m (new lexer (of_string " ( abc ) ")) (fun res s -> Parsed ((res, s), None)) with
  | Parsed ((str, _), _) -> Printf.printf "Parsed: %s\n" str
  | _ -> Printf.printf "Failed.\n"
  end;
