open Ostap
open Regexp
open Printf

let _ =
  let print = printUnique in
  let letter = Test ("letter", fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) in
  
  let quote   = Test ("quote"  , fun c -> c = '\'' || c = '"'                           ) in
  let noquote = Test ("noquote", fun c -> c != '\'' && c != '"'                         ) in

  let string          = Bind ("S", Juxt [Bind ("Q", quote); Aster noquote; Arg "Q"]) in
  let stringBeforeLetter = Juxt [string; Before letter]                                 in

  let m0 = matchAllStr string             in
  let m1 = matchAllStr stringBeforeLetter in

  printf "Matching \"string\" against \"\"abc\" and the rest\"\n";
  print ["Q"; "S"] (m0 (Ostream.fromString "\"abc\" and the rest"));
  printf "Matching \"stringBeforeLetter\" against \"\"abc\" and the rest\":\n";
  print ["Q"; "S"] (m1 (Ostream.fromString "\"abc\" and the rest"));
  printf "Matching \"stringBeforeLetter\" against \"\"abc\"and the rest\":\n";
  print ["Q"; "S"] (m1 (Ostream.fromString "\"abc\"and the rest"))
;;