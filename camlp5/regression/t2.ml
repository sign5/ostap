(*
 * Test006: simplest ocamlyard test.
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
  object (self : 'self)
    inherit stream s as super
    val ws = regexp "[' ''\n''\t']+"
    val ident = regexp "[a-zA-Z]\([a-zA-Z0-9]\)*"
    method getIDENT :
        'a . (string -> 'self -> ('a, 'self) result) -> ('a, 'self) result =
      fun k ->
        let str = of_chars s in
        let p' =
          if string_match ws str p then p + String.length (matched_string str)
          else p
        in
        if string_match ident str p' then
          let m = matched_string str in k m {< p = p' + String.length m >}
        else emptyResult
    method look :
        'b .
          string -> (string -> 'self -> ('b, 'self) result) ->
            ('b, 'self) result =
      fun cs k -> super#look cs k
    method getEOF :
        'a . (string -> 'self -> ('a, 'self) result) -> ('a, 'self) result =
      fun k ->
        let str = of_chars s in
        let p' =
          if string_match ws str p then p + String.length (matched_string str)
          else p
        in
        if p' = String.length str then k "EOF" self else emptyResult
  end

  module T :
  sig
   val list1 : (< chrs : char list; correctErrors : string;
              equal : 'self -> bool; errors : Errors.t;
              getEOF : 'b.
                         (string -> 'self -> ('b, 'self) Result.result) ->
                         ('b, 'self) Result.result;
              getIDENT : 'a.
                           (string -> 'self -> ('a, 'self) Result.result) ->
                           ('a, 'self) Result.result;
              look : 'b.
                       string ->
                       (string -> 'self -> ('b, 'self) Result.result) ->
                       ('b, 'self) Result.result;
              lookChar : 'b.
                           char ->
                           (char -> 'self -> ('b, 'self) Result.result) ->
                           ('b, 'self) Result.result;
              pos : int; rest : string; str : string; .. >
            as 'self) ->
           (string list, string list, 'self) Types.k ->
           (string list, 'self) Result.result
  end =
    struct
let list1 (_ostap_stream : #stream) =
  Ostap.Combinators.seq
    (fun ((_ostap_stream : < getIDENT : 'a. (string -> 'self -> ('a, 'self) result) -> ('a, 'self) result; .. > as 'self)) ->
       _ostap_stream#getIDENT)
    (fun (hd as _1) ->
       Ostap.Combinators.map (fun (tl as _0) -> hd :: tl)
         (Ostap.Combinators.T.many
            (Ostap.Combinators.seq
               (fun ((_ostap_stream : #stream)) -> _ostap_stream#look ",")
               (fun _
                    ((_ostap_stream : < getIDENT : 'a. (string -> 'self -> ('a, 'self) result) -> ('a, 'self) result; .. > as 'self)) ->
                  _ostap_stream#getIDENT))))
    _ostap_stream
    end
    open T
(* let list = ostap (hd:list1  tl:(-";" list1)* {hd :: tl}) *)

  module T2 :
  sig
   val list : (< chrs : char list; correctErrors : string;
              equal : 'self -> bool; errors : Errors.t;
              getEOF : 'b.
                         (string -> 'self -> ('b, 'self) Result.result) ->
                         ('b, 'self) Result.result;
              getIDENT : 'a.
                           (string -> 'self -> ('a, 'self) Result.result) ->
                           ('a, 'self) Result.result;
              look : 'b.
                       string ->
                       (string -> 'self -> ('b, 'self) Result.result) ->
                       ('b, 'self) Result.result;
              lookChar : 'b.
                           char ->
                           (char -> 'self -> ('b, 'self) Result.result) ->
                           ('b, 'self) Result.result;
              pos : int; rest : string; str : string; .. >
            as 'self) ->
           (string list list, string list list, 'self) Types.k ->
           (string list list, 'self) Result.result
  end =
    struct
let list (_ostap_stream : #stream) =
  Ostap.Combinators.seq list1
    (fun (hd as _1) ->
       (* Ostap.Combinators.map (fun (tl as _0) -> hd :: tl) *)
         (Ostap.Combinators.T.many list1))
    _ostap_stream
    end
    open T2
(* let m (_ostap_stream : #stream) =
  Ostap.Combinators.seq list
    (fun (_ as _0) ->
       Ostap.Combinators.map (fun _ -> _0)
         (fun ((_ostap_stream : < getEOF : 'a. (string -> 'self -> ('a, 'self) result) -> ('a, 'self) result; .. > as 'self)) ->
            _ostap_stream#getEOF))
    _ostap_stream
let _ =
  begin match
    m ((new lexer) (of_string "r,t , f , g ,     u, i; u, g "))
      (fun res s -> Parsed ((res, s), None))
  with
    Parsed ((str, _), _) ->
      Printf.printf "Parsed: %s\n"
        (List.fold_left (fun s l -> List.fold_left (^) s l) "" str)
  | _ -> Printf.printf "Failed.\n"
  end;
  match
    m ((new lexer) (of_string " abc; def "))
      (fun res s -> Parsed ((res, s), None))
  with
    Parsed ((str, _), _) ->
      Printf.printf "Parsed: %s\n"
        (List.fold_left (fun s l -> List.fold_left (^) s l) "" str)
  | _ -> Printf.printf "Failed.\n" *)
