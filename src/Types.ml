(*
 * Types: common types.
 * Copyright (C) 2016
 * Ekaterina Verbitskaja, St.Petersburg State University
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

 type ('a, 'b) tag = Parsed of 'a * 'b option | Failed of 'b option
 type ('b, 'c, 'stream) result = ('b * 'stream, 'c) tag

 let emptyResult = Failed None
 
 module K :
   sig
     type ('a, 'b, 'c, 'stream) t = 'a -> 'stream -> ('b, 'c, 'stream) result
     type ks

     val singleton : ('a, 'b, 'c, 'stream) t -> ks
     val add       : ('a, 'b, 'c, 'stream) t -> ks -> ks
     val fold      : (('a, 'b, 'c, 'stream) t -> ('b, 'c, 'stream) result -> ('b, 'c, 'stream) result) -> ks -> ('b, 'c, 'stream) result -> ('b, 'c, 'stream) result
     val empty     : ks
     val length    : ks -> int

   end =
   struct

     type ('a, 'b, 'c, 'stream) t = 'a -> 'stream -> ('b, 'c, 'stream) result

     module Ks = Set.Make (
       struct
 	type t = Obj.t

 	let compare x y = (Pervasives.compare : int -> int -> int) (Obj.magic x) (Obj.magic y)
       end
     )
     type ks = Ks.t

     let singleton k         = Ks.add (Obj.repr k) Ks.empty
     let add       k ks      = Ks.add (Obj.repr k) ks
     let fold      f ks acc  = Ks.fold (fun k acc -> f (Obj.magic k) acc) ks acc
     let empty               = Ks.empty
     let length      ks      = Ks.cardinal ks
   end

 type ('a, 'b, 'c, 'stream) k       = ('a, 'b, 'c, 'stream) K.t
 type ('a, 'b, 'c, 'stream) parser  = 'stream -> ('a, 'b, 'c, 'stream) k -> ('b, 'c, 'stream) result
 type ('a, 'b, 'c, 'stream) parser' =            ('a, 'b, 'c, 'stream) k -> ('b, 'c, 'stream) result

let bind result f =
  match result with
  | Parsed ((v, s), err) ->
      (match f v with
       | `Ok v'     -> Parsed ((v', s), err)
       | `Fail err' -> Failed (Some err')
      )
  | Failed x -> Failed x

let (<@>) : ('b, 'c, 'stream) result -> ('b, 'c, 'stream) result -> ('b, 'c, 'stream) result =
  fun res1 res2 ->
    match res1, res2 with
    | Parsed ((res, x), opt1), Failed opt2        -> Parsed ((res, x), opt1)
    | Failed opt1,        Parsed ((res, x), opt2) -> Parsed ((res, x), opt1)
    | Parsed ((res, x), opt1), Parsed ((_, _), opt2)   -> Parsed ((res, x), opt1)
    | Failed opt1,        Failed opt2        -> Failed (opt1)
