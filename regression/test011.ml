open Ostap
open Regexp
open Printf
open Deterministic

let _ =
   let print = printUnique in
   let letter   = Test ("letter"  , fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) in
   let noletter = Test ("noletter", fun c -> (c < 'a' || c > 'z') && (c < 'A' || c > 'Z')    ) in
   let digit    = Test ("digit"   , fun c -> c >= '0' && c <= '9'                            ) in
   let nodig    = Test ("nodig"   , fun c -> c < '0' || c > '9'                              ) in
   let noid     = Test ("noid"    , fun c -> (c < 'a' || c > 'z') && (c < 'A' || c > 'Z') && (c < '0' || c > '9')) in
   let quote    = Test ("quote"   , fun c -> c = '\'' || c = '"'                            ) in
   let noquote  = Test ("noquote" , fun c -> c != '\'' && c != '"'                          ) in
      
   let string             = Bind ("S", Juxt [Bind ("Q", quote); Aster noquote; Arg "Q"]) in
   let stringBeforeLetter = Juxt [string; Before letter] in
   let letters            = Aster (letter) in
   let digits             = Bind ("d", Aster (digit)) in
   let ident              = Juxt [Bind ("ID", Juxt [letter; Aster (Alter [letter; digit])]); Bind ("NEXT", Alter [noid; EOS])] in
   let identId            = Juxt [ident; Arg "ID"] in
   let identDoubleBefore  = Juxt [ident; Before noletter; Before nodig] in
   let identDigits        = Aster (Juxt [ident; digits]) in
   let lettersDigits      = Aster (Juxt [Bind("id", letters); digits]) in
   let lettersOrDigits    =       Alter [Bind("id", letters); digits]  in

   let m0 = matchAllStr (Diagram.make string            ) in
   let m1 = matchAllStr (Diagram.make stringBeforeLetter) in
   let m2 = matchAllStr (Diagram.make digits            ) in
   let m3 = matchAllStr (Diagram.make identId           ) in
   let m4 = matchAllStr (Diagram.make identDoubleBefore ) in
   let m5 = matchAllStr (Diagram.make identDigits       ) in
   let m6 = matchAllStr (Diagram.make lettersDigits     ) in
   let m7 = matchAllStr (Diagram.make lettersOrDigits   ) in
   let m8 = matchAllStr (Diagram.make letters           ) in

   printf "Matching \"string\" against \"\"abc\" and the rest\"\n";
   print ["Q"; "S"]     (m0 (Ostream.fromString "\"abc\" and the rest"));
   printf "Matching \"stringBeforeLetter\" against \"\"abc\" and the rest\":\n";
   print ["Q"; "S"]     (m1 (Ostream.fromString "\"abc\" and the rest"));
   printf "Matching \"stringBeforeLetter\" against \"\"abc\"and the rest\":\n";
   print ["Q"; "S"]     (m1 (Ostream.fromString "\"abc\"and the rest"));
   printf "Matching \"digits\" against \"\"; should match\n";
   print ["d"]          (m2 (Ostream.fromString ""));
   printf "Matching \"identId\" against \"abc and the rest\"; should not match\n";
   print ["ID"; "NEXT"] (m3 (Ostream.fromString "abc and the rest"));
   printf "Matching \"identId\" against \"abc abc and the rest\"; should match \"abc abc\"\n";
   print ["ID"; "NEXT"] (m3 (Ostream.fromString "abc abc and the rest"));
   printf "Matching \"identDoubleBefore\" against \"abc 1and the rest\"; should not match\n";
   print ["ID"; "NEXT"] (m4 (Ostream.fromString "abc 1and the rest"));
   printf "Matching \"identDigits\" against \"1 and the rest\"; should not remove \"1\" from stream\n";
   print ["ID"; "NEXT"] (m5 (Ostream.fromString "1 and the rest"));
   printf "Matching \"identDigits\" against \"abc and the rest\"; should match the whole string and return [ID]=\"abcandtherest\"\n";
   print ["ID"; "NEXT"] (m5 (Ostream.fromString "abc and the rest"));
   printf "Matching \"lettersDigits\" against \"abc and the rest\"; should match \"abc\"\n";
   print ["id"; "d"]    (m6 (Ostream.fromString "abc and the rest"));
   printf "Matching \"lettersDigits\" against \"abc1 and the rest\"; should match \"abc1\"\n";
   print ["id"; "d"]    (m6 (Ostream.fromString "abc1 and the rest"));
   printf "Matching \"lettersOrDigits\" against \"!!! and the rest\"; should match \"\" and return id=[], d=[]\n";
   print ["id"; "d"]    (m7 (Ostream.fromString "!!! and the rest"));
   printf "Matching \"letters\" against \"5 and the rest\"; should match \"\" and return id=[], d=[]\n";
   print ["id"; "d"]    (m8 (Ostream.fromString "5 and the rest"));
;;