open Ostap
open Regexp
open Printf
open Deterministic

let _ =
   let a = Test ("a", fun c -> c = 'a') in
   let b = Test ("b", fun c -> c = 'b') in
   let c = Test ("c", fun c -> c = 'c') in
   let acab = Alter [Juxt [a;c]; Juxt [a;b]] in
   let acab_binds = Alter [Bind ("a", Juxt [a;c]); Bind ("b", Juxt [a;b])] in
   let acab_args = Juxt [Bind ("ab", acab_binds); Alter [Juxt [Arg "ab"; Arg "a"]; Juxt [Arg "ab"; Arg "b"]]] in
   let letter = Test ("letter", fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) in
   let quote   = Test ("quote"  , fun c -> c = '\'' || c = '"'                           ) in
   let noquote = Test ("noquote", fun c -> c != '\'' && c != '"'                         ) in
   let string  = Bind ("S", Juxt [Bind ("Q", quote); Aster noquote; Arg "Q"]             ) in
   let stringNotLetter = Juxt [string; Before letter]                                      in
   let test = Juxt [c; Before (Alter [a; b])] in
   List.iter
      (fun expr ->
         let module RD = Diagram in
         let d = RD.make expr in
(*         printf "%s\n" (Diagram.toDOT d);*)
         let module A = ASCIIStreamChar in
         let module D = DetNFA(A) in
(*         D.printTable (D.make d);*)
         let min = D.minimize d in
(*         D.printTable min;*)
         printf "%s\n" (D.toDOT min)
      )
      [acab; acab_binds; acab_args; string; stringNotLetter; test]