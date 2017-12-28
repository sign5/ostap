open Lazy

let of_chars chars =
  let buf = Buffer.create 16 in
    List.iter (Buffer.add_char buf) chars;
    Buffer.contents buf

let of_string s =
  let n = String.length s in
  let rec loop i =
    if i = n then [] else s.[i] :: loop (i+1)
  in
  loop 0

type ('b, 'stream) result = ('b * 'stream) list

(* Functional objects: http://caml.inria.fr/pub/docs/manual-ocaml/objectexamples.html#sec36 *)
class stream (s : char list) =
  object (self : 'self)

    val p = 0

    method pos       = p
    method str       = of_chars s
    method chrs      = s
    method rest      =
      let rec f l p' = if (p' = 0) then l else f (List.tl l) (p' - 1)
      in of_chars (f s p)

    method equal : stream -> bool =
      fun s' -> (s = s' # chrs) && (p = s' # pos)

    method look : 'b . char -> (char -> 'self -> ('b, 'self) result) -> ('b, 'self) result =
      fun c k ->
      try
        if c = List.nth s p
        then k c {< p = p + 1 >}
        else []
      with _ -> []

    method getEOF : 'b . (unit -> 'self -> ('b, 'self) result) -> ('b, 'self) result =
      fun k ->
        if p = List.length s
        then k () self
        else []
  end

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

let success : 'a -> ('a, 'b) parser = fun a k s -> k a s
let failure :       ('a, 'b) parser = fun   k s -> []

let empty = fun k -> success [] k

let seq : ('a, 'b) parser -> ('a -> ('c, 'b) parser) -> ('c, 'b) parser =
  fun x y k -> x (fun a -> y a k)

let (|>) = seq

let memoresult : ('a, 'b) parser' -> ('a, 'b) parser' =
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
	              K.fold (fun k acc -> acc @ (k a s)) !ks [])
             |  _ -> []
          ))
      else (ks := K.add k !ks;
	    List.flatten (List.map (fun (s, a) -> (k a s)) !ss ))

let memo : ('a, 'b) parser -> ('a, 'b) parser =
  fun f ->
    let table : (stream, ('a, 'b) parser') Hashtbl.t = Hashtbl.create 16 in
    fun k s ->
      try Option.get (Hashtbl.fold (fun s' p' acc -> match acc with
                                                     | Some _                   -> acc
					             | None when (s # equal s') -> Some p'
					             | _                        -> None
				   ) table None) k
      with _ ->
        let r = memoresult @@ (fun k -> f k s) in
        Hashtbl.add table s r; r k

let alt : ('a, 'b) parser -> ('a, 'b) parser -> ('a, 'b) parser =
  fun x y -> memo (fun k s -> (x k s) @ (y k s))

let (<|>) = alt

let fix : (('a, 'b) parser -> ('a, 'b) parser) -> ('a, 'b) parser =
  fun f -> let rec p = lazy (f (fun t -> force p t)) in force p

let lift : ('a -> 'b) -> ('b, stream) result -> ('b, stream) result =
  fun f rs -> List.map (fun (a, s) -> (f a, s)) rs

let map : ('a -> 'c) -> ('a, 'b) parser -> ('c, 'b) parser =
  fun f p -> p |> (fun a -> success (f a))

let rec manyFold : (('a -> 'c -> 'c) -> 'c -> ('a, 'b) parser -> ('c, 'b) parser) =
  fun f init p -> (empty |> fun _ -> success init) <|>
                  (p                 |> (fun xp  ->
	           manyFold f init p |> (fun xps ->
		   success (f xp xps))))
(*
let many : ('a, 'b) parser -> ('a, 'b) parser =
  fun p ->
    (manyFold (fun acc x -> fun l -> acc (x :: l)) (fun x -> x) p) --> (fun t -> t [])

let (<*>) = many
*)
let someFold : (('a -> 'c -> 'c) -> 'c -> ('a, 'b) parser -> ('c, 'b) parser) =
  fun f init p -> p                 |> (fun xp  ->
	          manyFold f init p |> (fun xps ->
	          success (f xp xps)))
(*
let some ('a, 'b) parser -> ('a, 'b) parser =
  fun p ->
    (someFold (fun acc x -> fun l -> acc (x :: l)) (fun x -> x) p) --> (fun t -> t [])

let (<+>) = some
*)

let _ =
  let test1 = ((fun k stream' -> (stream' # look 'a') k) |> (fun xa ->
	       (fun k stream' -> (stream' # look 'b') k) |> (fun xb ->
		success (xa :: xb :: [])))) (fun res s -> [(res, s)]) in
    match test1 (new stream @@ of_string @@ "ab") with
    | []            -> Printf.printf "Test01.1 failed. Not parsed.\n";
    | (res, s) :: _ -> Printf.printf "Test01.1 succeeded. Parsed with result : %s and rest of the string : %s\n" (of_chars res) (s # rest);
    match test1 (new stream @@ of_string @@ "ac") with
    | []            -> Printf.printf "Test01.2 succeeded. Not parsed.\n"
    | (res, s) :: _ -> Printf.printf "Test01.2 failed. Parsed with result : %s and rest of the string : %s\n" (of_chars res) (s # rest)

let _ =
  let test2 = ((fun k stream' -> (stream' # look 'a') k) <|> (fun k stream' -> (stream' # look 'b') k)) (fun res s -> [(res, s)]) in
    match test2 (new stream @@ of_string @@ "b") with
    | []            -> Printf.printf "Test02.1 failed. Not parsed.\n";
    | (res, s) :: _ -> Printf.printf "Test02.1 succeeded. Parsed with result : %c and rest of the string : %s\n" res (s # rest);
    match test2 (new stream @@ of_string @@ "c") with
    | []            -> Printf.printf "Test02.2 succeeded. Not parsed.\n"
    | (res, s) :: _ -> Printf.printf "Test02.2 failed. Parsed with result : %c and rest of the string : %s\n" res (s # rest)

let _ =
  let pretest3 = (fix (fun p -> let neWord = (fun k stream' -> (stream' # look 'a') k) |> (fun xa ->
					     p                                         |> (fun xp ->
					     success (xa :: xp))) in
                                    neWord <|> ((fun k stream' -> (stream' # look 'a') k) |> fun xa ->
				                 success (xa :: [])))) in
  let test3 = (pretest3 |> (fun res -> (fun k stream' -> (stream' # getEOF) k) |> (fun _ -> success res))) (fun res s -> [(res, s)]) in
  match test3 (new stream @@ of_string @@ "aaa") with
  | []            -> Printf.printf "Test03.1 failed. Not parsed.\n";
  | (res, s) :: _ -> Printf.printf "Test03.1 succeeded. Parsed with result : %s and rest of the string : %s\n" (of_chars res) (s # rest);
  match test3 (new stream @@ of_string @@ "aaaba") with
  | []            -> Printf.printf "Test03.2 succeeded. Not parsed.\n"
  | (res, s) :: _ -> Printf.printf "Test03.2 failed. Parsed with result : %s and rest of the string : %s\n" (of_chars res) (s # rest)

let terminal : char -> (char, 'b) parser =
  fun c k stream' -> (stream' # look c) k

let eof : (unit, 'b) parser =
  fun k stream' -> (stream' # getEOF) k

let _ =
  let pretest4 = (fix (fun p -> let neWord = p            |> (fun xp ->
				             terminal 'a' |> (fun xa ->
				             success (xp @ (xa :: [])))) in
				    neWord <|> (terminal 'a' |> fun xa ->
				                success (xa :: [])))) in
  let test4 = (pretest4 |> (fun res -> eof |> (fun _ -> success res))) (fun res s -> [(res, s)]) in
  match test4 (new stream @@ of_string @@ "aaa") with
  | []            -> Printf.printf "Test04.1 failed. Not parsed.\n";
  | (res, s) :: _ -> Printf.printf "Test04.1 succeeded. Parsed with result : %s and rest of the string : %s\n" (of_chars res) (s # rest);
  match test4 (new stream @@ of_string @@ "aaaba") with
  | []            -> Printf.printf "Test04.2 succeeded. Not parsed.\n"
  | (res, s) :: _ -> Printf.printf "Test04.2 failed. Parsed with result : %s and rest of the string : %s\n" (of_chars res) (s # rest)

let _ =
  let pretest5 = (fix (fun p -> let neWord = p            |> (fun xp1    ->
				             terminal '+' |> (fun xplus  ->
					     p            |> (fun xp2    ->
				             success (xp1 @ (xplus :: xp2))))) in
				    neWord <|> (terminal 'a' |> fun xa ->
				                success (xa :: []))))  in
  let test5 = (pretest5 |> (fun res -> eof |> (fun _ -> success res))) (fun res s -> [(res, s)]) in
  match test5 (new stream @@ of_string @@ "a+a+a+a") with
  | []            -> Printf.printf "Test05.1 failed. Not parsed.\n";
  | (res, s) :: _ -> Printf.printf "Test05.1 succeeded. Parsed with result : %s and rest of the string : %s\n" (of_chars res) (s # rest);
  match test5 (new stream @@ of_string @@ "a+a+a+b+a") with
  | []            -> Printf.printf "Test05.2 succeeded. Not parsed.\n"
  | (res, s) :: _ -> Printf.printf "Test05.2 failed. Parsed with result : %s and rest of the string : %s\n" (of_chars res) (s # rest)

let _ =
  let pretest6 = (fix (fun p -> let aWord = terminal 'a' |> (fun _  ->
				            p            |> (fun xp ->
					    terminal 'a' |> (fun _  ->
				            success (1 + xp)))) and
				     bWord = terminal 'b' |> (fun _  ->
				   	     p            |> (fun xp ->
				   	     terminal 'b' |> (fun _  ->
				   	     success (1 + xp)))) and
				     cWord = terminal 'c' |> (fun _  ->
				             p            |> (fun xp ->
				      	     terminal 'c' |> (fun _  ->
				      	     success (1 + xp)))) in
				     aWord <|> bWord <|> cWord     <|>
				     (empty |> fun _ -> success 0) <|>
				     ((terminal 'a' <|> terminal 'b' <|> terminal 'c') |> fun _ -> success 1))) in
  let test6 = (pretest6 |> (fun res -> eof |> (fun _ -> success res))) (fun res s -> [(res, s)]) in
  match test6 (new stream @@ of_string @@ "abbccbba") with
  | []            -> Printf.printf "Test06.1 failed. Not parsed.\n"
  | (res, s) :: _ -> Printf.printf "Test06.1 succeeded. Parsed with result : %d and rest of the string : %s\n" res (s # rest);

  match test6 (new stream @@ of_string @@ "abcba") with
  | []            -> Printf.printf "Test06.2 failed. Not parsed.\n";
  | (res, s) :: _ -> Printf.printf "Test06.2 succeeded. Parsed with result : %d and rest of the string : %s\n" res (s # rest);
  match test6 (new stream @@ of_string @@ "abccbaa") with
  | []            -> Printf.printf "Test06.3 succeeded. Not parsed.\n"
  | (res, s) :: _ -> Printf.printf "Test06.3 failed. Parsed with result : %d and rest of the string : %s\n" res (s # rest)

let _ =
  let rec pretest7 k = (fix (fun p -> let plusWord = p            |> (fun xp    ->
				                     terminal '+' |> (fun xplus ->
				      	             mulli        |> (fun xm    ->
				      	             success (xp @ (xplus :: xm))))) in
				          mulli <|> plusWord)) @@ k and
	  mulli    k = (fix (fun p -> let multWord = p            |> (fun xp    ->
					             terminal '*' |> (fun xmult ->
					      	     primary      |> (fun xpr   ->
					      	     success (xp @ (xmult :: xpr))))) in
					primary <|> multWord)) @@ k and
	  primary  k = ((terminal 'a' <|> terminal 'b' <|> terminal 'c') |> fun xt -> success (xt :: [])) @@ k in
  let test7 = (pretest7 |> (fun res -> eof |> (fun _ -> success res))) (fun res s -> [(res, s)]) in
  match test7 (new stream @@ of_string @@ "a+b+c") with
  | []            -> Printf.printf "Test07.1 failed. Not parsed.\n";
  | (res, s) :: _ -> Printf.printf "Test07.1 succeeded. Parsed with result : %s and rest of the string : %s\n" (of_chars res) (s # rest);
  match test7 (new stream @@ of_string @@ "a*b+c") with
  | []            -> Printf.printf "Test07.2 failed. Not parsed.\n";
  | (res, s) :: _ -> Printf.printf "Test07.2 succeeded. Parsed with result : %s and rest of the string : %s\n" (of_chars res) (s # rest);
  match test7 (new stream @@ of_string @@ "a+b+*c") with
  | []            -> Printf.printf "Test07.3 succeeded. Not parsed.\n";
  | (res, s) :: _ -> Printf.printf "Test07.3 failed. Parsed with result : %s and rest of the string : %s\n" (of_chars res) (s # rest);
  match test7 (new stream @@ of_string @@ "a+b+") with
  | []            -> Printf.printf "Test07.4 succeeded. Not parsed.\n"
  | (res, s) :: _ -> Printf.printf "Test07.4 failed. Parsed with result : %s and rest of the string : %s\n" (of_chars res) (s # rest)

let _ =
  let expr n =
    let rec e k = (fix (fun s -> let plusWord = s              |> (fun xs    ->
				                (terminal '+' <|>
						 terminal '-') |> (fun xplus ->
				      	        m              |> (fun xm    ->
				      	        success (xs @ (xplus :: xm))))) in
			             m <|> plusWord)) @@ k and
	    m k = (fix (fun s -> let multWord = s              |> (fun xs    ->
					        (terminal '*' <|>
						 terminal '/') |> (fun xmult ->
					      	p              |> (fun xpr   ->
					      	success (xs @ (xmult :: xpr))))) in
				     p <|> multWord)) @@ k and
	    p k = (n <|> (terminal '(' |> (fun xbr1 ->
	  		  e            |> (fun xe   ->
	  		  terminal ')' |> (fun xbr2 ->
	  		  success ((xbr1 :: xe) @ (xbr2 :: []))))))) @@ k in
  (e |> (fun res -> eof |> (fun _ -> success res))) (fun res s -> [(res, s)]) in
  match expr (terminal 'n' |> fun xt -> success (xt :: [])) (new stream @@ of_string @@ "n*(n-n)") with
  | []            -> Printf.printf "Test08.1 failed. Not parsed.\n";
  | (res, s) :: _ -> Printf.printf "Test08.1 succeeded. Parsed with result : %s and rest of the string : %s\n" (of_chars res) (s # rest);
  match expr ((terminal 'a' <|> terminal 'b') |> fun xt -> success (xt :: [])) (new stream @@ of_string @@ "a*(b+a)") with
  | []            -> Printf.printf "Test08.2 failed. Not parsed.\n";
  | (res, s) :: _ -> Printf.printf "Test08.2 succeeded. Parsed with result : %s and rest of the string : %s\n" (of_chars res) (s # rest);
  match expr (terminal 'n' |> fun xt -> success (xt :: [])) (new stream @@ of_string @@ "n*(n--n)") with
  | []            -> Printf.printf "Test08.3 succeeded. Not parsed.\n";
  | (res, s) :: _ -> Printf.printf "Test08.3 failed. Parsed with result : %s and rest of the string : %s\n" (of_chars res) (s # rest);
  match expr ((terminal 'a' <|> terminal 'b') |> fun xt -> success (xt :: [])) (new stream @@ of_string @@ "a+b+") with
  | []            -> Printf.printf "Test08.4 succeeded. Not parsed.\n"
  | (res, s) :: _ -> Printf.printf "Test08.4 failed. Parsed with result : %s and rest of the string : %s\n" (of_chars res) (s # rest)

let _ =
  let rec pretest9 k = (fix (fun p -> (p |> (fun xp1 ->
	                               p |> (fun xp2 ->
				       p |> (fun xp3 ->
				       success (xp1 @ xp2 @ xp3))))) <|>
				      (p |> (fun xp1 ->
				       p |> (fun xp2 ->
				       success (xp1 @ xp2)))) <|>
				      (terminal 'b' |> fun xb -> success (xb :: [])) <|>
				      (empty |> fun _ -> success []))) @@ k in
  let test9 = (pretest9 |> (fun res -> eof |> (fun _ -> success res))) (fun res s -> [(res, s)]) in
  match test9 (new stream @@ of_string @@ "bbbbbbbbb") with
  | []            -> Printf.printf "Test09.1 failed. Not parsed.\n";
  | (res, s) :: _ -> Printf.printf "Test09.1 succeeded. Parsed with result : %s and rest of the string : %s\n" (of_chars res) (s # rest);
  match test9 (new stream @@ of_string @@ "bbbcb") with
  | []            -> Printf.printf "Test09.2 succeeded. Not parsed.\n"
  | (res, s) :: _ -> Printf.printf "Test09.2 failed. Parsed with result : %s and rest of the string : %s\n" (of_chars res) (s # rest)

let _ =
  let pretest10 = manyFold (fun b bs -> b :: bs) [] (terminal 'b') in
  let test10 = (pretest10 |> (fun res -> eof |> (fun _ -> success res))) (fun res s -> [(res, s)]) in
  match test10 (new stream @@ of_string @@ "bbbbbbbbb") with
  | []            -> Printf.printf "Test10.1 failed. Not parsed.\n";
  | (res, s) :: _ -> Printf.printf "Test10.1 succeeded. Parsed with result : %s and rest of the string : %s\n" (of_chars res) (s # rest);
  match test10 (new stream @@ of_string @@ "bbbcb") with
  | []            -> Printf.printf "Test10.2 succeeded. Not parsed.\n";
  | (res, s) :: _ -> Printf.printf "Test10.2 failed. Parsed with result : %s and rest of the string : %s\n" (of_chars res) (s # rest);
  match test10 (new stream @@ of_string @@ "") with
  | []            -> Printf.printf "Test10.3 failed. Not parsed.\n"
  | (res, s) :: _ -> Printf.printf "Test10.3 succeeded. Parsed with result : %s and rest of the string : %s\n" (of_chars res) (s # rest)

let _ =
  let pretest11 = someFold (fun b bs -> b :: bs) [] (terminal 'b') in
  let test11 = (pretest11 |> (fun res -> eof |> (fun _ -> success res))) (fun res s -> [(res, s)]) in
  match test11 (new stream @@ of_string @@ "bbbbbbbbb") with
  | []            -> Printf.printf "Test11.1 failed. Not parsed.\n";
  | (res, s) :: _ -> Printf.printf "Test11.1 succeeded. Parsed with result : %s and rest of the string : %s\n" (of_chars res) (s # rest);
  match test11 (new stream @@ of_string @@ "bbbcb") with
  | []            -> Printf.printf "Test11.2 succeeded. Not parsed.\n";
  | (res, s) :: _ -> Printf.printf "Test11.2 failed. Parsed with result : %s and rest of the string : %s\n" (of_chars res) (s # rest);
  match test11 (new stream @@ of_string @@ "") with
  | []            -> Printf.printf "Test11.3 succeeded. Not parsed.\n"
  | (res, s) :: _ -> Printf.printf "Test11.3 failed. Parsed with result : %s and rest of the string : %s\n" (of_chars res) (s # rest)

let _ =
  let expr n =
    let rec e k = (fix (fun s -> let plusWord = s            |> (fun xs    ->
				                (terminal '+' <|>
						 terminal '-') |> (fun xplus ->
				      	        m              |> (fun xm    ->
				      	        success (if xplus = '+' then xs + xm else xs - xm)))) in
			             m <|> plusWord)) @@ k and
	    m k = (fix (fun s -> let multWord = s              |> (fun xs    ->
					        (terminal '*' <|>
						 terminal '/') |> (fun xmult ->
					      	p              |> (fun xpr   ->
					      	success (if xmult = '*' then xs * xpr else xs / xpr)))) in
				     p <|> multWord)) @@ k and
	    p k = (n <|> (terminal '(' |> (fun _  ->
	  		  e            |> (fun xe ->
	  		  terminal ')' |> (fun _  ->
	  		  success xe))))) @@ k in
  (e |> (fun res -> eof |> (fun _ -> success res))) (fun res s -> [(res, s)]) in
  match expr ((terminal '0' <|> terminal '1' <|> terminal '2' <|> terminal '3') |> (fun t -> success ((Char.code t) - 48))) (new stream @@ of_string @@ "1+2*3") with
  | []            -> Printf.printf "Test14.1 failed. Not parsed.\n";
  | (res, s) :: _ -> Printf.printf "Test14.1 succeeded. Parsed with result : %d and rest of the string : %s\n" res (s # rest);
