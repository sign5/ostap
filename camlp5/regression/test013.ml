open Re_str
open Ostap
open Types
open Matcher
open Printf
open Lazy
open Combinators

class lexer s =
  let skip  = Skip.create [Skip.whitespaces " \n\t\r"] in
  let const = Re_str.regexp "[0-9]+" in
  object (self)

    inherit Matcher.t s

    method skip p c = skip s p c
    method getCONST : 'b . (Token.t -> 'self -> ('self, 'b, Reason.t) result) -> ('self, 'b, Reason.t) result = self#get "constant" const

  end
(*
let left  f c x y = f (c x) y
let right f c x y = c (f x y)
let id x = x

let opnd = ostap("1" {1})

let ops = [|1, [ostap("+"), (+)]|]

let rec expr f =
  let n      = Array.length ops in
  let nona i = fst ops.(i)      in
  let ops'' x =
    Array.map
      (fun (1, list) ->
        (* let g = match assoc with `Lefta | `Nona -> left | `Righta -> right in *)
        1, altl (List.map (fun (oper, sema) -> ostap (!(oper) {left sema})) list)
      )
      x in
  let op i = snd (ops'' ops).(i) in

  let ostap (
    inner[l][c] : !(f (ostap (x:inner[l][c] o:!(op l) y:opnd {o id x y} | opnd)))
  )
  in
  ostap (inner[0][id] (";" !(expr f))?) *)

(* ostap (
  primary:
    c:CONST {`N c} ;

  exp:
     e:exp -"+" p:primary {`E2 (e, p)}
   | p:primary {`E1 p} ;

  main: exp -EOF
) *)

(* ostap (main : !(expr id) -EOF) *)

let default =
  [| `Righta, ((fun x -> x+1, 1), [":=", (fun x -> x+1), (fun _ _ _ -> 1)]) |]

let right f c x a y = c (f x a y)

let expr ops opnd atr =
  let ops =
    Array.map
      (fun (assoc, (atrs, list)) ->
        let g = match assoc with `Righta -> right in
        assoc = `Righta, (atrs, altl (List.map (fun (oper, sema) -> ostap (!(oper) {g sema})) list))
      )
      ops
  in
  let op   = snd (snd ops.(0)) in
  let id x = x                in
  let ostap ( inner[atr]: x:opnd[atr] o:op y:opnd[atr] {o id x atr y})
  in
  ostap (inner[atr])

ostap (
  pparse[atr]: !(expr (Array.map (fun (a, (atr, l)) -> a, (atr, List.map (fun (s, _, f) -> ostap (- $(s)), f) l)) default) (primary) atr);
  primary[atr]: CONST
)

ostap (
  main: !(pparse 1)? {["is"]}
)

let _ =
  let rec print r =
    match r with
    | `N p -> "n"
    | `E1 e -> "E[" ^ (print e) ^ "]"
    | `E2 (i, e) -> "E[" ^ (print i) ^ "+" ^ (print e) ^ "]"
  in
  match main (new lexer "1+1+1+1; 1+1") (fun res s -> Parsed ((res, s), None)) with
  | Parsed ((b, _), _) -> Printf.printf "Parsed.\n"
  (* | Parsed ((b, _), _) -> Printf.printf "Parsed: %s\n" (print b) *)
  | Failed _ -> Printf.printf "Not parsed.\n"
