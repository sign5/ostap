open Ostap
open Regexp
open Printf

let _ =
  let print = printUnique in
  let letter   = Test ("letter"  , fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) in
  let noletter = Test ("noletter", fun c -> (c < 'a' || c > 'z') && (c < 'A' || c > 'Z')    ) in
  let noid     = Test ("noid"    , fun c -> (c < 'a' || c > 'z') && (c < 'A' || c > 'Z') && (c < '0' || c > '9')) in
  let digit    = Test ("digit"   , fun c -> c >= '0' && c <= '9'                            ) in
  let nodig    = Test ("nodig"   , fun c -> c < '0' || c > '9'                              ) in

  let ident    = Juxt [Bind ("ID", Juxt [letter; Aster (Alter [letter; digit])]); Bind ("NEXT", Alter [noid; EOS])] in

  let identDoubleBefore = Juxt [ident; Before noletter; Before nodig] in
(*
  List.iter
    (fun expr ->
       printf "%s\n" (Diagram.toDOT (Diagram.make expr))
    )
    [identDoubleBefore];
*)
  printf "Matching \"identDoubleBefore\" against \"abc 1and the rest\"; should not match\n";
  print ["ID"; "NEXT"] (matchAllStr identDoubleBefore (Ostream.fromString "abc 1and the rest"));
;;