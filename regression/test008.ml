open Ostap
open Regexp
open Printf

let _ =
  let print = printUnique in
  let letter = Test ("letter", fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) in
  let noid   = Test ("noid"  , fun c -> (c < 'a' || c > 'z') && (c < 'A' || c > 'Z') && (c < '0' || c > '9')) in
  let digit  = Test ("digit" , fun c -> c >= '0' && c <= '9'                            ) in

  let ident  = Juxt [Bind ("ID", Juxt [letter; Aster (Alter [letter; digit])]); Bind ("NEXT", Alter [noid; EOS])] in

  let letters          = Aster (letter) in
  let digits           = Aster (digit) in
  let lettersDigits    = Aster (Juxt [Bind("id", letters); Bind("d", digits)]) in
  let lettersOrDigits  =       Alter [Bind("id", letters); Bind("d", digits)]  in
  let identDigits      = Aster (Juxt [ident; digits]) in

(*
  List.iter 
    (fun expr ->
       printf "%s\n" (Diagram.toDOT (Diagram.make expr))
    )
    [identDigits; lettersDigits; lettersOrDigits];
*)
  printf "Matching \"identDigits\" against \"abc and the rest\"; should match the whole string and return [ID]=\"abcandtherest\"\n";
  print ["ID"; "NEXT"] (matchAllStr identDigits (Ostream.fromString "abc and the rest"));
  printf "Matching \"lettersDigits\" against \"abc and the rest\"; should match \"abc\"\n";
  print ["id"; "d"] (matchAllStr lettersDigits (Ostream.fromString "abc and the rest"));
  printf "Matching \"lettersDigits\" against \"abc1 and the rest\"; should match \"abc1\"\n";
  print ["id"; "d"] (matchAllStr lettersDigits (Ostream.fromString "abc1 and the rest"));
  printf "Matching \"lettersOrDigits\" against \"!!! and the rest\"; should match \"\" and return id=[], d=[]\n";
  print ["id"; "d"] (matchAllStr lettersOrDigits (Ostream.fromString "!!! and the rest"));
  printf "Matching \"letters\" against \"5 and the rest\"; should match \"\" and return id=[], d=[]\n";
  print ["id"; "d"] (matchAllStr letters (Ostream.fromString "5 and the rest"));

;;