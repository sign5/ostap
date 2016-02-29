open Ostap
open Regexp
open Printf

let _ =
  let print = printUnique in
  let letter = Test ("letter", fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) in
  let noid   = Test ("noid"  , fun c -> (c < 'a' || c > 'z') && (c < 'A' || c > 'Z') && (c < '0' || c > '9')) in
  let digit  = Test ("digit" , fun c -> c >= '0' && c <= '9'                            ) in
  let nodig  = Test ("nodig" , fun c -> c < '0' || c > '9'                              ) in
  let ws     = Test ("ws"    , fun c -> c = ' '                                         ) in
  let nows   = Test ("nows"  , fun c -> c != ' '                                        ) in

  let ident  = Juxt [Bind ("ID", Juxt [letter; Aster (Alter [letter; digit])]); Bind ("NEXT", Alter [noid; EOS])] in
  let decim  = Juxt [Bind ("DC", Plus digit); Bind ("NEXT", Alter [nodig; EOS])]                                  in
 
  let item   = Alter [ident; decim]                     in
  let wss    = Juxt  [Aster ws; Bind ("NEXT", Alter [nows; EOS])] in
   
  let item   = matchAllStr item in
  let wss    = matchAllStr wss  in

  let analyseString str =
    printf "Analysing string \"%s\"\n" str;
    let rec inner s =
      if Ostream.endOf s then ()
      else 
        let m = item s in
        if Ostream.endOf m then ()
        else begin          
          printf "matched:\n"; 
          print ["ID"; "DC"; "NEXT"] m;
          let (s', m) = Ostream.hd m in
          let s'' = Ostream.concat (Ostream.fromString (m "NEXT")) s' in
          let m   = wss s'' in          
          inner (if Ostream.endOf m then s'' else let (s', m) = Ostream.hd m in Ostream.concat (Ostream.fromString (m "NEXT")) s')
        end
    in
    inner (Ostream.fromString str);
    printf "End of analysis\n"
  in
  analyseString "123abc";
  analyseString "123 abc def 12 3 4 5 d d";
  analyseString "123 abc ddd ccc";
  analyseString "123";
  analyseString "abc"
;;