(*
 * Test009: simplest ocamlyard test.
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

class lexer (s : string) =
  object (self : 'self)

    val ws    = regexp "[' ''\n''\t']+"
    val ident = regexp "[a-zA-Z]\([a-zA-Z0-9]\)*"
    val p = 0
    val s = s

    method private pos = p
    method private str = s
    method equal : 'self -> bool =
      fun s' -> (s = s' # str) && (p = s' # pos)

    method getIDENT : 'b . (String.t -> 'self -> ('self, 'b, Reason.t) result) -> ('self, 'b, Reason.t) result =
      fun k ->
        let p =
          if string_match ws s p
          then p + (String.length (matched_string s))
          else p
        in
        if string_match ident s p
        then
          let m = matched_string s in
          k m {< p = p + String.length m >}
        else
          emptyResult

    method look : 'b . String.t -> (String.t -> 'self -> ('self, 'b, Reason.t) result) -> ('self, 'b, Reason.t) result =
      fun x k ->
        let p =
          if string_match ws s p
          then p + (String.length (matched_string s))
          else p
        in
        if string_match (regexp (quote x)) s p
        then
          let m = matched_string s in
          k m {< p = p + String.length m >}
        else emptyResult

    method getEOF : 'b . (String.t -> 'self -> ('self, 'b, Reason.t) result) -> ('self, 'b, Reason.t) result =
      fun k ->
        let p =
          if string_match ws s p
          then p + (String.length (matched_string s))
          else p
        in
        if p = String.length s
        then k "EOF" {< p = p >}
        else emptyResult
  end

module X =
struct
  let parse _ostap_stream =
    Ostap.Combinators.alt
      (fun
        (_ostap_stream :
           <
           look: 'b .
                   String.t ->
         ('alook -> 'self -> ('self, 'b, 'c) Types.result) ->
         ('self, 'b, 'c) Types.result   ;.. >
         as 'self)
        -> _ostap_stream#look ",")
      (fun
        (_ostap_stream :
           <
           look: 'b .
                   String.t ->
         ('alook -> 'self -> ('self, 'b, 'c) Types.result) ->
         ('self, 'b, 'c) Types.result   ;.. >
         as 'self)
        -> _ostap_stream#look ",-") _ostap_stream
end

  let (_fakename1, _fakename2) =
    let _generated_fixpoint _f1 _f2 =
      let rec _p1 =
        lazy
          (let _table = Hashtbl.create 16 in
           fun _param1 ->
           fun _s ->
             match Hashtbl.fold
                     (fun _param'1 ->
                        fun p' ->
                        fun acc ->
                          match acc with
                          | Some _ -> acc
                          | None when true && (_param1 == _param'1) ->
                            Some p'
                          | _ -> None) _table None
             with
             | None ->
               let _r =
                 _f1 (fun _t -> Lazy.force_val _p1 _t)
                   (fun _t -> Lazy.force_val _p2 _t) _param1 in
               (Hashtbl.add _table _param1 _r; _r _s)
             | Some x -> x _s)
      and _p2 =
        lazy
          (let _table = Hashtbl.create 16 in
           fun _s ->
             match Hashtbl.fold
                     (fun () ->
                        fun p' ->
                        fun acc ->
                          match acc with
                          | Some _ -> acc
                          | None when true -> Some p'
                          | _ -> None) _table None
             with
             | None ->
               let _r =
                 _f2 (fun _t -> Lazy.force_val _p1 _t)
                   (fun _t -> Lazy.force_val _p2 _t) in
               (Hashtbl.add _table () _r; _r _s)
             | Some x -> x _s) in
      ((fun _t -> Lazy.force_val _p1 _t), (fun _t -> Lazy.force_val _p2 _t)) in
    let _fakename1 list m elem =
      Ostap.Combinators.seq elem
        (fun (hd as _1) ->
           Ostap.Combinators.map (fun (tl as _0) -> hd :: tl)
             (Ostap.Combinators.many
                (Ostap.Combinators.seq X.parse (fun _ -> elem))))
    and _fakename2 list m =
      Ostap.Combinators.seq
        (list
           (fun _ostap_stream ->
              (fun
                (_ostap_stream :
                   <
                   getIDENT: 'b .
                               ('aIDENT ->
                                'self -> ('self, 'b, 'c) Types.result)
                 -> ('self, 'b, 'c) Types.result   ;.. >
                 as 'self)
                -> _ostap_stream#getIDENT) _ostap_stream))
        (fun (_ as _0) ->
           Ostap.Combinators.map (fun _ -> _0)
             (fun
               (_ostap_stream :
                  <
                  getEOF: 'b .
                            ('aEOF -> 'self -> ('self, 'b, 'c) Types.result)
                -> ('self, 'b, 'c) Types.result   ;.. >
                as 'self)
               -> _ostap_stream#getEOF)) in
    _generated_fixpoint _fakename1 _fakename2 in
  let list _param1 _s = _fakename1 _param1 _s
and m  _s = _fakename2 _s 


(*
module X =
  struct

    let parse = ostap ("," | ",-")

  end

ostap (
  list[elem] : hd:elem tl:(- !(X.parse) elem)* {hd :: tl};
  m : list[ostap (IDENT)] -EOF
)
 *)
let _ =
  begin match m (new lexer "r,-t , f , g ,     u, i ") (fun res s -> Parsed ((res, s), None)) with
  | Parsed ((str, _), _) ->
      Printf.printf "Parsed: %s\n" (List.fold_left (^) "" str)
  | _ -> Printf.printf "Failed.\n"
  end;
  begin match m (new lexer " abc; def ") (fun res s -> Parsed ((res, s), None)) with
  | Parsed ((str, _), _) ->
      Printf.printf "Parsed: %s\n" (List.fold_left (^) "" str)
  | _ -> Printf.printf "Failed.\n"
  end;
