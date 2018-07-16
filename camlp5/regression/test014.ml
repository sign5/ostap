open Re_str
open Ostap
open Types
open Matcher
open Printf

class lexer (str :  string) =
  let const = regexp "[0-9]+" in
  object (self : 'self) inherit stream str as super

    method getCONST : 'b . (string -> 'self -> ('self, 'b, Reason.t) result) -> ('self, 'b, Reason.t) result =
      fun k ->
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
  match main (new lexer "1-(2+3)+4") (fun res s -> Parsed ((res, s), None)) with
  | Parsed ((b, _), _) -> Printf.printf "Parsed: %s\n" (print b)
  | Failed _ -> Printf.printf "Not parsed:\n"
