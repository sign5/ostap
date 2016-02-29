open Ostap
open Regexp
open Printf

let _ =
  let print = printUnique in
  let letter = Test ("letter", fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) in
  let noid   = Test ("noid"  , fun c -> (c < 'a' || c > 'z') && (c < 'A' || c > 'Z') && (c < '0' || c > '9')) in
  let digit  = Test ("digit" , fun c -> c >= '0' && c <= '9'                            ) in

  let ident  = Juxt [Bind ("ID", Juxt [letter; Aster (Alter [letter; digit])]); Bind ("NEXT", Alter [noid; EOS])] in

  let digits           = Aster (digit) in
  let identDigits      = Aster (Juxt [ident; digits]) in
(*
  List.iter 
    (fun expr ->
       printf "%s\n" (Diagram.toDOT (Diagram.make expr))
    )
    [identDigits];
*)
  printf "Matching \"identDigits\" against \"1 and the rest\"; should not remove \"1\" from stream\n";
  print ["ID"; "NEXT"] (matchAllStr identDigits (Ostream.fromString "1 and the rest"));
;;
