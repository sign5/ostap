(*
 * Util: predefined Ostap utilities.
 * Copyright (C) 2006-2009
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

open Combinators
open Matcher
open Printf

module Ostap =
  struct

    module Combinators = Combinators

  end

ostap (
  keyword[name]: @(name ^ "\\b" : name)
)


let (~$) = keyword

ostap (
  listByWith[delim][item][f][x]: h:item result:(-delim item) * with{f x h}{f} {result}
)

ostap (
  listBy[delim][item]: h:item t:(-delim item)* {h::t}
)

ostap (
  listBy1[delim][item]: h:item t:(-delim item)+ {h::t}
)

ostap (
  listWith[item][f][x]: listByWith[ostap (",")][item][f][x]
)

ostap (
  list: listBy[ostap (",")]
)

ostap (
  list0ByWith[delim][item][f][x]: h:item result:(-delim item) * with{f x h}{f} {result} | empty {x}
)

ostap (
  list0By[delim][item]: listBy[delim][item] | empty {[]}
)

ostap (
  list0With[item][f][x]: list0ByWith[ostap (",")][item][f][x]
)

ostap (
  list0: list0By[ostap (",")]
)

let left  f c x y = f (c x) y
let right f c x y = c (f x y)

ostap (
  id[x]: x
)

(* Infix helpers *)
module Infix =
struct
   type ('expr, 'atr, 'm, 'stream, 'b, 'c) ops  = [`Lefta | `Righta | `Nona] *
                                                  (('atr -> ('atr * 'atr)) *
                                                  (('m, 'stream, 'b, 'c) Types.parserr *
                                                  (string * ('expr -> 'atr -> 'expr -> 'expr)) list))

   let compare_ops ((_, (_, (_, xs))) : ('e, 'a, 'm, 'stream, 'b, 'c) ops) ((_, (_, (_, ys))) : ('e, 'a, 'm, 'stream, 'b, 'c) ops) =
     Pervasives.compare (List.map fst xs) (List.map fst ys)

   module StringMap  = Map.Make (String)
   module PointerMap =
      struct

       include Map.Make (
         struct
           type t = Obj.t

           let compare x y = compare_ops (Obj.magic x) (Obj.magic y)
         end
       )

       let find (x : ('e, 'a, 'm, 'stream, 'b, 'c) ops) (m : 'v t) = find (Obj.repr x) m
       let add (x : ('e, 'a, 'm, 'stream, 'b, 'c) ops) (v : 'v) (m : 'v t) = add (Obj.repr x) v m
     end

  type ('e, 'a, 'm, 'stream, 'b, 'c) t = ('e, 'a, 'm, 'stream, 'b, 'c) ops * (((('e, 'a, 'm, 'stream, 'b, 'c) ops) option) PointerMap.t) * (((('e, 'a, 'm, 'stream, 'b, 'c) ops) option) PointerMap.t) * ((('e, 'a, 'm, 'stream, 'b, 'c) ops) StringMap.t) * ((('e, 'a, 'm, 'stream, 'b, 'c) ops) array)

  let left  f c x a y = f (c x) a y
  let right f c x a y = c (f x a y)
  let g assoc = match assoc with `Lefta | `Nona -> left | `Righta -> right
  let toOstap s = ostap (- $(s))

  let singleton a s (sem, atr) =
    let newp = Obj.magic (a, (atr, (ostap (- $(s) {g a sem}), [s, sem]))) in
    newp,
    (PointerMap.add newp None PointerMap.empty),
    (PointerMap.add newp None PointerMap.empty),
    (StringMap.add s newp StringMap.empty),
    ([| |])

  let name infix =
    let b = Buffer.create 64 in
    Buffer.add_string b "__Infix_";
    Seq.iter (fun c -> Buffer.add_string b (string_of_int @@ Char.code c)) @@ String.to_seq infix;
    Buffer.contents b

  let next   (_, _, nexts, _, _) l = match PointerMap.find (Obj.magic l) nexts with Some x -> x
  let isLast (_, _, nexts, _, _) l = match PointerMap.find (Obj.magic l) nexts with None -> true | _ -> false
  let start  (start, _, _, _, _)   = Obj.magic start
  let arr    (_, _, _, _, arr)     = arr
  let setArr (start, prevs, nexts, nodes, _) arr = (start, prevs, nexts, nodes, arr)

  let find_op (start, prevs, nexts, nodes, arr) op cb ce =
    match StringMap.find_opt op nodes with
    | None -> ce ()
    | Some p -> cb p (start, prevs, nexts, nodes, arr)

  let no_op op coord = `Fail (Printf.sprintf "infix ``%s'' not found in the scope at %s" op (Msg.Coord.toString coord))

  let at coord op newop (sem, _) infix =
    find_op infix op
      (fun p (start, prevs, nexts, nodes, arr) ->
         let (assoc, (atr, (o, l))) = Obj.magic p in
         let l = (newop, sem) :: l in
         let newp = Obj.magic (assoc, (atr, (alt o (ostap (- $(newop) {g assoc sem})), l))) in
         let newStart = match PointerMap.find p prevs with None -> newp | Some x -> start in
         let newPrevs = PointerMap.add newp (PointerMap.find p prevs) (match PointerMap.find p nexts with None -> prevs | Some next -> PointerMap.add next (Some newp) prevs) in
         let newNexts = PointerMap.add newp (PointerMap.find p nexts) (match PointerMap.find p prevs with None -> nexts | Some prev -> PointerMap.add prev (Some newp) nexts) in
         let newNodes = List.fold_right (fun (s, _) map -> StringMap.add s newp map) l nodes in
         `Ok (newStart, newPrevs, newNexts, newNodes, arr)
      )
      (fun _ -> no_op op coord)

  let before coord op newop assoc (sem, atr) infix =
    find_op infix op
      (fun p (start, prevs, nexts, nodes, arr) ->
         let newp = Obj.magic (assoc, (atr, (ostap (- $(newop) {g assoc sem}), [newop, sem]))) in
         let newStart = match PointerMap.find p prevs with None -> newp | Some x -> start in
         let newPrevs = PointerMap.add newp (PointerMap.find p prevs) (PointerMap.add p (Some newp) prevs) in
         let newNexts = PointerMap.add newp (Some p) (match PointerMap.find p prevs with None -> nexts | Some prev -> PointerMap.add prev (Some newp) nexts) in
         let newNodes = StringMap.add newop newp nodes in
         `Ok (newStart, newPrevs, newNexts, newNodes, arr)
      )
      (fun _ -> no_op op coord)

  let after coord op newop assoc (sem, atr) infix =
    find_op infix op
      (fun p (start, prevs, nexts, nodes, arr) ->
         let newp = Obj.magic (assoc, (atr, (ostap (- $(newop) {g assoc sem}), [newop, sem]))) in
         let newStart = start in
         let newPrevs = PointerMap.add newp (Some p) (match PointerMap.find p nexts with None -> prevs | Some next -> PointerMap.add next (Some newp) prevs) in
         let newNexts = PointerMap.add newp (PointerMap.find p nexts) (PointerMap.add p (Some newp) nexts) in
         let newNodes = StringMap.add newop newp nodes in
         `Ok (newStart, newPrevs, newNexts, newNodes, arr)
      )
      (fun _ -> no_op op coord)

  let createArray (start, prevs, nexts, nodes, _) as infix =
    let rec loop cur list =
      if isLast infix cur then list
      else
        loop (next infix cur) (cur :: list)
    in Array.of_list (List.rev (loop (Obj.magic start) []))

end

let expr f ops opnd =
  let ops =
    Array.map
      (fun (assoc, list) ->
        let g = match assoc with `Lefta | `Nona -> left | `Righta -> right in
        assoc = `Nona, altl (List.map (fun (oper, sema) -> ostap (!(oper) {g sema})) list)
      )
      ops
  in
  let n      = Array.length ops in
  let op   i = snd ops.(i)      in
  let nona i = fst ops.(i)      in
  let id   x = x                in
  let ostap (
    inner[l][c]: f[ostap (
      {n = l                } => x:opnd {c x}
    | {n > l && not (nona l)} => x:inner[l+1][id] b:(-o:op[l] inner[l][o c x])? {
        match b with None -> c x | Some x -> x
      }
    | {n > l && nona l} => x:inner[l+1][id] b:(op[l] inner[l+1][id])? {
        c (match b with None -> x | Some (o, y) -> o id x y)
      })]
  )
  in
  ostap (inner[0][id])

let read name =
  let inch = open_in_bin name in
  let len  = in_channel_length inch in
  let buf  = Bytes.make len ' ' in
  really_input inch buf 0 len;
  close_in inch;
  Bytes.unsafe_to_string buf

module Lexers =
  struct

    let isKeyword keywords =
      let module S = Set.Make (String) in
      let s = List.fold_left (fun s k -> S.add k s) S.empty keywords in
      (fun i -> S.mem i s)

    class checkKeywords keywords =
      let k = isKeyword keywords in
      object
	method private keyword = k
      end

    class virtual genericIdent regexp name keywords s =
      let regexp = Re_str.regexp regexp in
      object(self : 'self)
	inherit checkKeywords keywords
	method virtual get      : 'b. String.t -> Re_str.regexp -> (Token.t -> 'self -> ('self, 'b, Reason.t) Types.result) -> ('self, 'b, Reason.t) Types.result
  method private getIdent : 'b. (String.t -> 'self -> ('self, 'b, Reason.t) Types.result) -> ('self, 'b, Reason.t) Types.result =
   fun k -> self#get name regexp
       (fun t s ->
          let r = Token.repr t in
          if self#keyword r
          then Types.failWith (new Reason.t (Msg.make "%0 expected" [|name|] (Token.loc t)))
          else k r s)
      end

    class virtual uident keywords s =
      object inherit genericIdent "[A-Z]\([a-zA-Z_0-9]\)*\\b" "u-identifier" keywords s as ident
	method getUIDENT : 'b. (String.t -> 'self -> ('self, 'b, Reason.t) Types.result) -> ('self, 'b, Reason.t) Types.result = ident#getIdent
      end

    class virtual lident keywords s =
      object inherit genericIdent "[a-z]\([a-zA-Z_0-9]\)*\\b" "l-identifier" keywords s as ident
	method getLIDENT : 'b. (String.t -> 'self -> ('self, 'b, Reason.t) Types.result) -> ('self, 'b, Reason.t) Types.result = ident#getIdent
      end

    class virtual ident keywords s =
      object inherit genericIdent "[a-zA-Z]\([a-zA-Z_0-9]\)*\\b" "identifier" keywords s as ident
	method getIDENT : 'b. (String.t -> 'self -> ('self, 'b, Reason.t) Types.result) -> ('self, 'b, Reason.t) Types.result = ident#getIdent
      end

    class virtual decimal s =
      let regexp = Re_str.regexp "-?[0-9]+" in
      object(self : 'self)
        method virtual get : 'b. String.t -> Re_str.regexp -> (Token.t -> 'self -> ('self, 'b, Reason.t) Types.result) -> ('self, 'b, Reason.t) Types.result
        method getDECIMAL  : 'b. (int -> 'self -> ('self, 'b, Reason.t) Types.result) -> ('self, 'b, Reason.t) Types.result =
          fun k -> self#get "decimal constant" regexp (fun t s -> k (int_of_string (Token.repr t)) s)
      end

    class virtual string s =
      let regexp = Re_str.regexp (*"\"\([^\"]\|\\\"\)*\""*) "\"[^\"]*\"" in
      object(self : 'self)
        method virtual get : 'b. String.t -> Re_str.regexp -> (Token.t -> 'self -> ('self, 'b, Reason.t) Types.result) -> ('self, 'b, Reason.t) Types.result
        method getSTRING   : 'b. (String.t -> 'self -> ('self, 'b, Reason.t) Types.result) -> ('self, 'b, Reason.t) Types.result =
          fun k -> self#get "decimal constant" regexp (fun t s -> k (Token.repr t) s)
      end

    class virtual char s =
      let regexp = Re_str.regexp "'\([^']\|\\'\)'" in
      object(self : 'self)
        method virtual get : 'b. String.t -> Re_str.regexp -> (Token.t -> 'self -> ('self, 'b, Reason.t) Types.result) -> ('self, 'b, Reason.t) Types.result
        method getCHAR     : 'b. (Char.t -> 'self -> ('self, 'b, Reason.t) Types.result) -> ('self, 'b, Reason.t) Types.result =
          fun k -> self#get "character constant" regexp (fun t s -> k ((Token.repr t).[1]) s)
      end

    class skip skippers s =
      object inherit Matcher.t s
	val skipper = Skip.create skippers s
	method skip = skipper
      end

  end

  let parse l p =
    Combinators.unwrap (p l (fun res s -> Types.Parsed ((res, s), None)))
      (fun x -> `Ok x)
      (fun x ->
         match x with
         | Some err ->
           let [loc, m :: _] = err#retrieve (`First 1) (`Desc) in
           let m =  match m with `Msg m -> m | `Comment (s, _) -> Msg.make s [||] loc in
           `Fail (Msg.toString m)
         | None -> `Fail "Oh, no error explanation"
      )
