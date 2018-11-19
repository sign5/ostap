open Re_str
open Ostap
open Types
open Matcher
open Printf
open Lazy

class lexer (str :  string) =
  object (self : 'self) inherit stream str as super

    val ws    = regexp "[' ''\n''\t']+"
    val ident = regexp "[a-zA-Z]\([a-zA-Z0-9]\)*"
    val const = Re_str.regexp "[0-9]+"

    method getCONST : 'b . (string -> 'self -> ('self, 'b, Reason.t) result) -> ('self, 'b, Reason.t) result =
      fun k ->
        let p' =
          if string_match ws str p
          then p + (String.length (matched_string str))
          else p
        in
        if string_match const str p'
        then
          let m = matched_string str in
          k m {< p = p' + String.length m >}
        else
          emptyResult

    method look : 'b . string -> (string -> 'self -> ('self, 'b, Reason.t) result) -> ('self, 'b, Reason.t) result =
      fun cs k -> (*super # look cs k*)
        try
          let p =
            if string_match ws str p
            then p + (String.length (matched_string str))
            else p
          in
          let l = String.length cs in
          let m = String.sub str p l in
          let p = p + l in
          if cs = m
          then k m {< p = p >}
          else emptyResult
        with Invalid_argument _ -> emptyResult

    method getEOF : 'b . (string -> 'self -> ('self, 'b, Reason.t) result) -> ('self, 'b, Reason.t) result =
      fun k ->
        let p' =
          if string_match ws str p
          then p + (String.length (matched_string str))
          else p
        in
        if p' = String.length str
        then k "EOF" self
        else emptyResult
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
  match main (new lexer "1+2+3") (fun res s -> Parsed ((res, s), None)) with
  | Parsed ((b, _), _) -> Printf.printf "Parsed: %s\n" (print b)
  | Failed _ -> Printf.printf "Not parsed.\n"
