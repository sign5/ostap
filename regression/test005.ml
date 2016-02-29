open Ostap
open Regexp
open Printf

let _ =
  let print = printUnique in
  let letter = Test ("letter", fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) in
  let noid   = Test ("noid"  , fun c -> (c < 'a' || c > 'z') && (c < 'A' || c > 'Z') && (c < '0' || c > '9')) in
  let digit  = Test ("digit" , fun c -> c >= '0' && c <= '9'                            ) in

  let ident  = Juxt [Bind ("ID", Juxt [letter; Aster (Alter [letter; digit])]); Bind ("NEXT", Alter [noid; EOS])] in

  let identId          = Juxt [ident; Arg "ID"] in
  let identBeforeId    = Juxt [ident; Before Arg "ID"] in
(*
  List.iter
    (fun expr ->
       printf "%s\n" (Diagram.toDOT (Diagram.make expr))
    )
    [identId; identBeforeId];
*)
  printf "Matching \"identId\" against \"abc and the rest\"; should not match\n";
  print ["ID"; "NEXT"] (matchAllStr identId (Ostream.fromString "abc and the rest"));
  printf "Matching \"identId\" against \"abc abc and the rest\"; should match \"abc abc\"\n";
  print ["ID"; "NEXT"] (matchAllStr identId (Ostream.fromString "abc abc and the rest"));
(*
  printf "Matching \"identBeforeId\" against \"abc and the rest\"; should not match\n";
  print ["ID"; "NEXT"] (matchAllStr identBeforeId (Ostream.fromString "abc and the rest"));
*)
  printf "Matching \"identBeforeId\" against \"abc abc and the rest\"; should match \"abc \"\n";
  print ["ID"; "NEXT"] (matchAllStr identBeforeId (Ostream.fromString "abc abc and the rest"));
;;