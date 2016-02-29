open Ostap
open Regexp
open Printf

let _ =
  let letter = Test ("letter", fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) in
  let noid   = Test ("noid"  , fun c -> (c < 'a' || c > 'z') && (c < 'A' || c > 'Z') && (c < '0' || c > '9')) in
  let digit  = Test ("digit" , fun c -> c >= '0' && c <= '9'                            ) in
  let nodig  = Test ("nodig" , fun c -> c < '0' || c > '9'                              ) in
  let ws     = Test ("ws"    , fun c -> c = ' '                                         ) in
  let nows   = Test ("nows"  , fun c -> c != ' '                                        ) in

  let ident  = Juxt [Bind ("ID", Juxt [letter; Aster (Alter [letter; digit])]); Bind ("NEXT", Alter [noid; EOS])] in
  let decim  = Juxt [Bind ("DC", Plus digit); Bind ("NEXT", Alter [nodig; EOS])]                                  in

  let idents          = Juxt [Bind ("1", ident); ws; Arg "1"] in
  let identNotDigit   = Juxt [ident; Before (digit)] in
  let identLookahead  = Juxt [ident; Before EOS] in
  let doubleLookahead = Juxt [decim; Before identLookahead] in
  let letterOrDigit   = Alter  [letter; digit] in
  let test            = Juxt [ws; Before letterOrDigit] in

  List.iter 
    (fun expr ->
       printf "%s\n" (Diagram.toDOT (Diagram.make expr))
    )
    [doubleLookahead; identLookahead; identNotDigit; idents; ident; decim; letterOrDigit; test]
;;