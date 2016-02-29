open Ostap
open Regexp
open Printf

let _ =
  let print = printUnique in
  let digit  = Test ("digit" , fun c -> c >= '0' && c <= '9'                            ) in

  let digits = Bind ("d", Aster (digit)) in

  printf "Matching \"digits\" against \"\"; should match\n";
  print ["d"] (matchAllStr digits (Ostream.fromString ""));
;;