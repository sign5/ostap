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

 open Errors
 open Matcher
 open Result

 module K :
   sig
     type ('a, 'b) t = 'a -> stream -> ('b, stream) result
     type ks

     val singleton : ('a, 'b) t -> ks
     val add       : ('a, 'b) t -> ks -> ks
     val fold      : (('a, 'b) t -> ('b, stream) result -> ('b, stream) result) -> ks -> ('b, stream) result -> ('b, stream) result
     val empty     : ks
     val length    : ks -> int

   end =
   struct

     type ('a, 'b) t = 'a -> stream -> ('b, stream) result

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

 type ('a, 'b) k       = ('a, 'b) K.t
 type ('a, 'b) parser  = ('a, 'b) k -> stream -> ('b, stream) result
 type ('a, 'b) parser' = ('a, 'b) k ->           ('b, stream) result

let bind result f =
  match result with
  | Parsed ((v, s), err) ->
      (match f v with
       | `Ok v'     -> Parsed ((v', s), err)
       | `Fail err' -> Failed (Some err')
      )
  | Failed x -> Failed x
