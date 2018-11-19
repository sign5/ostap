open Re_str
open Ostap
open Types
open Matcher
open Printf

module H = Hashtbl.Make(struct
                         type t = int * int
                         let equal (f0, p0) (f1, p1) = f0 == f1 && p0 = p1
                         let hash = Hashtbl.hash
                       end)


class lexer (str :  string) =
  object (self : 'self) inherit stream str as super

    val ws    = regexp "[' ''\n''\t']+"

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
  l:
     a:p -"." -"x" {`Lp a}
   | -"x" {`Lx} ;

  p:
     a:p -"(" -"n" -")" {`Pn a}
   | a:l {`Pl a} ;

  main: l -EOF
)

let _ =
  let rec print r =
    match r with
    | `Lp a -> "L[" ^ (print a) ^ ".x]"
    | `Lx -> "L[x]"
    | `Pn a -> "P[" ^ (print a) ^ "(n)]"
    | `Pl a -> "P[" ^ (print a) ^ "]"
  in
  match main (new lexer "x(n)(n).x(n).x") (fun res s -> Parsed ((res, s), None)) with
  | Parsed ((b, _), _) -> Printf.printf "Parsed: %s\n" (print b)
  | Failed _ -> Printf.printf "Not parsed:\n"
