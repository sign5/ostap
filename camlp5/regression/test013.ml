open Re_str
open Ostap
open Types
open Result
open Errors
open Matcher
open Printf
open Lazy

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
  primary:
    c:CONST {`N c} ;

  exp:
     e:exp -"+" p:primary {`E2 (e, p)}
   | p:primary {`E1 p} ;

  main: exp -EOF
)

let _ =
  let rec print r =
    match r with
    | `N p -> "n"
    | `E1 e -> "E[" ^ (print e) ^ "]"
    | `E2 (i, e) -> "E[" ^ (print i) ^ "+" ^ (print e) ^ "]"
  in
  match main (new lexer (of_string "1+2+3")) (fun res s -> Parsed ((res, s), None)) with
  | Parsed ((b, _), _) -> Printf.printf "Parsed: %s\n" (print b)
  | Failed _ -> Printf.printf "Not parsed.\n"
