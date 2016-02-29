open Ostap
open Regexp
open Printf
open Deterministic

let _ =
  let letter   = Test ("letter"  , fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) in
  let noletter = Test ("noletter", fun c -> (c < 'a' || c > 'z') && (c < 'A' || c > 'Z')    ) in
  let noid     = Test ("noid"    , fun c -> (c < 'a' || c > 'z') && (c < 'A' || c > 'Z') && (c < '0' || c > '9')) in
  let digit    = Test ("digit"   , fun c -> c >= '0' && c <= '9'                            ) in
  let nodig    = Test ("nodig"   , fun c -> c < '0' || c > '9'                              ) in
  let ws       = Test ("ws"      , fun c -> c = ' '                                         ) in

  let ident  = Juxt [Bind ("ID", Juxt [letter; Aster (Alter [letter; digit])]); Bind ("NEXT", Alter [noid; EOS])] in
  let decim  = Juxt [Bind ("DC", Plus digit); Bind ("NEXT", Alter [nodig; EOS])]                                  in

  let letters            = Aster (letter) in
  let digits             = Aster (digit) in
  let idents             = Juxt [Bind ("1", ident); ws; Arg "1"] in
  let identNotDigit      = Juxt [ident; Before (digit)] in
  let identLookahead     = Juxt [ident; Before EOS] in
  let doubleLookahead    = Juxt [decim; Before identLookahead] in
  let letterOrDigit      = Alter [letter; digit] in
  let testLookahead      = Juxt [ws; Before letterOrDigit] in
  let identId            = Juxt [ident; Arg "ID"] in
  let identDoubleBefore  = Juxt [ident; Before noletter; Before nodig] in
  let identDigits        = Aster (Juxt [ident; digits]) in (* one of the best examples *)
  let lettersDigitsBound = Aster (Juxt [Bind("id", letters); Bind("d", digits)]) in
  let lettersOrDigits    =       Alter [Bind("id", letters); Bind("d", digits)]  in
  let lettersDigits      = Aster (Juxt [letters; digits]) in

  List.iter 
    (fun expr ->
       let d = Diagram.make expr in
       (*printf "%s\n" (Diagram.toDOT d);*)
       let module D = DetNFA(ASCIIStreamChar) in
       printf "%s\n" (D.toDOT (D.minimize d))
    )
    [ doubleLookahead
    ; identLookahead
    ; identNotDigit
    ; idents
    ; ident
    ; decim
    ; letterOrDigit
    ; testLookahead
    ; identId
    ; identDoubleBefore
    ; identDigits
    ; lettersDigitsBound
    ; lettersDigits
    ; lettersOrDigits]
;;