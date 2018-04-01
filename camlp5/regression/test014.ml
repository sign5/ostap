open Re_str
open Ostap
open Types
open Result
open Errors
open Matcher
open Printf

class lexer (s : char list) =
  let const = regexp "[0-9]+" in
  object (self : 'self) inherit stream s as super

    method getCONST : 'b . (string -> 'self -> ('b, 'self) result) -> ('b, 'self) result =
      fun k ->
        let str = of_chars s in
	if string_match const str p
	then
          let m = matched_string str in
          k m {< p = p + String.length m >}
	else
          emptyResult
  end

ostap (
  n:
     c:CONST {`N c} ;

  t:
     -"(" a:e -")" {`TBr a}
   | a:n {`TN a} ;

  e:
     a:e -"+" b:t {`EAdd (a, b)}
   | a:e -"-" b:t {`ESub (a, b)}
   | a:t {`ET a} ;

  main: e -EOF
)

let _ =
  let rec print r =
    match r with
    | `N _ -> "n"
    | `TN a -> "T[" ^ (print a) ^ "]"
    | `TBr a -> "T[" ^ "(" ^ (print a) ^ ")" ^ "]"
    | `ET a -> "E[" ^ (print a) ^ "]"
    | `EAdd (a, b) -> "E[" ^ (print a) ^ "+" ^ (print b) ^ "]"
    | `ESub (a, b) -> "E[" ^ (print a) ^ "-" ^ (print b) ^ "]"
  in
  match main (new lexer (of_string "1-(2+3)+4")) (fun res s -> Parsed ((res, s), None)) with
  | Parsed ((b, _), _) -> Printf.printf "Parsed: %s\n" (print b)
  | Failed _ -> Printf.printf "Not parsed:\n"
