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

class lexer (str :  string) =
  object (self : 'self) inherit stream str as super

    val ws    = regexp "[' ''\n''\t']+"
    val ident = regexp "[a-zA-Z]\([a-zA-Z0-9]\)*"

    method getIDENT : 'b . (string -> 'self -> ('self, 'b, Reason.t) result) -> ('self, 'b, Reason.t) result =
      fun k ->
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
          emptyResult

    method look : 'b . string -> (string -> 'self -> ('self, 'b, Reason.t) result) -> ('self, 'b, Reason.t) result =
      fun cs k -> (*super # look cs k*)
        try
          let p =
            if string_match ws str p
            then p + (String.length (matched_string str))
            else p
          in
          let l = String.length cs in
          let m = String.sub str p l in
          let p = p + l in
          if cs = m
          then k m {< p = p >}
          else emptyResult
        with Invalid_argument _ -> emptyResult

    method getEOF : 'b . (string -> 'self -> ('self, 'b, Reason.t) result) -> ('self, 'b, Reason.t) result =
      fun k ->
        let p' =
          if string_match ws str p
          then p + (String.length (matched_string str))
          else p
        in
        if p' = String.length str
        then k "EOF" self
        else emptyResult
  end

let id = ostap (IDENT -EOF)

let _ =
  begin match id (new lexer "hasToBeParsed") (fun res s -> Parsed ((res, s), None)) with
  | Parsed ((str, s), _) -> Printf.printf "Parsed: %s\n" str
  | _ -> Printf.printf "Failed.\n"
  end;
  begin match id (new lexer "   123 ") (fun res s -> Parsed ((res, s), None)) with
  | Parsed ((str, _), _) -> Printf.printf "Parsed: %s\n" str
  | _ -> Printf.printf "Failed.\n"
  end;
