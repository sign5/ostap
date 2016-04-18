open Combinators

let memo = 
  fun p (s : #MemoParser.t) -> s#memoize p

let fix p s = 
  let x' = ref None in  
  let rec fix p s = p (fix p) s in
  let p x s = 
    match !x' with
    | None   -> x' := Some (fun s -> memo x s); p x s 
    | Some x -> p x s 
  in
  fix p s

