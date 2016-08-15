open Ostap
open Types
open Matcher 
open Printf 


module H = Hashtbl.Make(struct
                         type t = int * int 
                         let equal (f0, p0) (f1, p1) = f0 == f1 && p0 = p1
                         let hash = Hashtbl.hash
                       end)

class lexer s =
  let skip  = Skip.create [Skip.whitespaces " \n\t\r"] in
  object (self)

    inherit Combinators.t s

    method skip p c = skip s p c
  end

let memo : (lexer, 'p, 'e) parse -> lexer -> (lexer, 'p, 'e) result = fun p s -> 
  s#memoize p

ostap (
  l:
     a:memo[p] -"." -"x" {`Lp a}
   | -"x" {`Lx} ;

  p: 
     a:memo[p] -"(" -"n" -")" {`Pn a}
   | a:memo[l] {`Pl a} ;

  main: memo[l] -EOF
)

let _ =
  let rec print r = 
    match r with 
    | `Lp a -> "L[" ^ (print a) ^ ".x]"
    | `Lx -> "L[x]"
    | `Pn a -> "P[" ^ (print a) ^ "(n)]"
    | `Pl a -> "P[" ^ (print a) ^ "]"
  in
  match main (new lexer "x(n)(n).x(n).x") with
  | Parsed ((b, _), _) -> Printf.printf "Parsed: %s\n" (print b)
  | Failed m -> Printf.printf "Not parsed:\n%s\n" (Reason.toString `All `Acc m)

