open Lazy
open Matcher
open Types
open Errors
open Result

let return =
  fun x k s -> k x s

let cast =
  function Failed x -> Failed x | _ -> invalid_arg "Ostap.cast"

let map =
  fun f p k s ->
    match p k s with
    | Parsed ((b, s'), e) -> Parsed ((f b, s'), e)
    | x -> cast x

let (-->) p f = map f p

let empty =
  fun k -> return () k

let fail =
 fun r k s -> Failed r

let lift =
  fun k s -> Parsed ((s, s), None)

let sink =
  fun p k s ->
    match p k s with
    | Parsed ((s, _), f) -> Parsed ((s, s), f)
    | Failed x           -> Failed x

let memoresult =
  fun p ->
    let ss : (stream * 'a) list ref = ref      [] in
    let ks :               K.ks ref = ref K.empty in
    fun k ->
      if K.length !ks = 0
      then (
        ks := K.singleton k;
        p (fun a s ->
             match List.find_all (fun (s', a') -> (a = a') && (s # equal s')) !ss with
	     | [] -> (ss := (s, a) :: !ss;
	              K.fold (fun k acc -> acc <@> (k a s)) !ks emptyResult)
             |  _ -> emptyResult
          ))
      else (ks := K.add k !ks;
	    List.fold_left (fun acc x -> match acc, x with
		                         | Parsed _,    _           -> acc
					 | Failed _,    Parsed _    -> x
					 | Failed opt1, Failed opt2 -> Failed (cmp opt1 opt2))
                           emptyResult
			   (List.map (fun (s, a) -> (k a s)) !ss))

let memo =
  fun f ->
    let table : (stream, ('a, 'b) parser') Hashtbl.t = Hashtbl.create 16 in
    fun k s ->
      match (Hashtbl.fold (fun s' p' acc -> match acc with
                                                     | Some _                   -> acc
					             | None when (s # equal s') -> Some p'
					             | _                        -> None
				   ) table None) with
        | None -> let r = memoresult @@ (fun k -> f k s) in
                  Hashtbl.add table s r; r k
        | Some x -> x k

let alt =
  fun x y -> memo (fun k s -> (x k s) <@> (y k s))

let (<|>) = alt

let seq =
  fun x y k -> x (fun a -> y a k)

let (|>) = seq

let opt =
  fun p k s -> let s' = Oo.copy s in
               let k' = fun a s -> (k (Some a) s) <@> (k None s') in
	       p k' s

let (<?>) = opt

let rec manyFold =
  fun f init p -> (empty |> fun _ -> return init) <|>
                  (p                 |> (fun xp  ->
	           manyFold f init p |> (fun xps ->
		   return (f xp xps))))
(*
let many : ('a, 'b) parser -> ('a, 'b list) parser =
  fun p ->
    (manyFold (fun x acc -> fun l -> acc (x :: l)) (fun x -> x) p) --> (fun t -> t [])

let (<*>) = many
*)
let someFold =
  fun f init p -> p                 |> (fun xp  ->
	          manyFold f init p |> (fun xps ->
	          return (f xp xps)))
(*
let some : ('a, 'b) parser -> ('a, 'b list) parser =
  fun p ->
    (someFold (fun x acc -> fun l -> acc (x :: l)) (fun x -> x) p) --> (fun t -> t [])

let (<+>) = some
*)
let guard =
  fun p f r k s ->
    match p k s with
    | (Parsed ((b, _), _) as x) ->
        if f b
        then x
        else Failed (match r with None -> None | Some r -> Some (r b))
    | y -> y
(*
let unwrap r f g =
match r with
| Parsed ((x, _), _) -> f x
| Failed x           -> g x
*)
let altl =
  fun l -> List.fold_left (<|>) (fail None) l

let fix =
  fun f -> let rec p = lazy (f (fun t -> force p t)) in force p
