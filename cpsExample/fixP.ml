open List
open Hashtbl
open Printf
open String
open Lazy

let of_string s =
  let n = String.length s in
  let rec loop i =
    if i = n then [] else s.[i] :: loop (i+1)
  in
  loop 0

module CPS =
  struct

  let success s = function k -> (k s)
  let failure s = function k -> ((*do nothing*))

  let memoresult res =
    let rs = ref [] in
    let ks = ref [] in
      function k -> (
        if (List.length !ks = 0)
        then begin
          ks := k::!ks;
          let ki = function t -> (if (not (List.mem t !rs))
                                  then begin
                                    rs := t::!rs;
                                    let f elem = elem t in
                                    List.iter f !ks
                                  end) in
          res () ki
        end
        else begin
          ks := k::!ks;
          List.iter k !rs
        end)

  let memo f =
    let table = Hashtbl.create 16 in
      function s -> (
        if (Hashtbl.mem table s)
        then begin
          Hashtbl.find table s
        end
        else begin
          add table s (memoresult (fun () -> f s));
          Hashtbl.find table s
        end
        )

  let eof =
    function
    | [] -> success []
    | s -> failure s

  let cpsterminal c =
    function
    | s::ss when s = c -> success ss
    | ss -> failure ss

  let cpsepsilon = success

  let cpsseq a b =
    function s ->
      (function k ->
        a s (function t -> b t k))

  let cpsalt2 a =
    let table = Hashtbl.create 16 in
    function b ->
      function s -> (
        if (Hashtbl.mem table s)
        then begin
          Hashtbl.find table s
        end
        else begin
          add table s (memoresult (fun () -> let x = lazy (b s) in
                                             (function k -> begin a s k; force x k end)));
          Hashtbl.find table s
        end
        )

  let cpsalt a b =
    memo
      (function s ->
        (function k -> begin a s k; b s k end))

  let fix2 f =
    let rec p = lazy ((f (function t -> force p t))) in force p

  let test1 x = cpsseq (cpsterminal 'a') (cpsterminal 'b') x
  in test1 (of_string "ab") (function s -> Printf.printf "Test1::success\n");
     test1 (of_string "cab") (function s -> Printf.printf "Test1.5::success\n");

  let test3 x = fix2 (function s -> cpsalt (cpsseq (cpsterminal 'a') s) (cpsterminal 'a')) x
  in cpsseq test3 eof (of_string "aaa") (function s -> Printf.printf "Test3::success\n");

  let test4 x = fix2 (function s -> cpsalt (cpsseq s (cpsterminal 'a')) (cpsterminal 'a')) x
  in cpsseq test4 eof (of_string "aaa") (function s -> Printf.printf "Test4::success\n");

  let test5 x = fix2 (function s -> cpsalt (cpsseq s (cpsseq (cpsterminal '+') s)) (cpsterminal 'a')) x
  in cpsseq test5 eof (of_string "a+a+a+a") (function s -> Printf.printf "Test5::success\n");

  let test6 x = fix2 (function s -> cpsalt (cpsseq (cpsterminal 'a') (cpsseq s (cpsterminal 'a')))
                                           (cpsalt (cpsseq (cpsterminal 'b') (cpsseq s (cpsterminal 'b')))
                                                   (cpsalt (cpsseq (cpsterminal 'c') (cpsseq s (cpsterminal 'c')))
                                                            cpsepsilon))) x
  in cpsseq test6 eof (of_string "acbbca") (function s -> Printf.printf "Test6::success\n");

  let rec test7 x   = fix2 (function s -> cpsalt mulli (cpsseq s (cpsseq (cpsterminal '+') mulli))) x
  and     mulli x   = fix2 (function s -> cpsalt primary (cpsseq s (cpsseq (cpsterminal '*') primary))) x
  and     primary x = cpsalt (cpsterminal 'a') (cpsalt  (cpsterminal 'b') (cpsterminal 'c')) x
  in cpsseq test7 eof (of_string "a+b+c") (function s -> Printf.printf "Test7::success\n");
     cpsseq test7 eof (of_string "a*b+c") (function s -> Printf.printf "Test7.5::success\n");

  let rec test8 x = fix2 (function s -> cpsalt (cpsseq s (cpsseq s s)) (cpsalt (cpsseq s s) (cpsalt (cpsterminal 'b') cpsepsilon))) x
  in cpsseq test8 eof (of_string "bbbbbb") (function s -> Printf.printf "Test8::success\n");

  let expr n =
    let rec e x = fix2 (function s -> cpsalt m (cpsseq s (cpsseq (cpsalt (cpsterminal '+') (cpsterminal '-')) m))) x
    and     m x = fix2 (function s -> cpsalt p (cpsseq s (cpsseq (cpsalt (cpsterminal '*') (cpsterminal '/')) p))) x
    and     p x = cpsalt n (cpsseq (cpsterminal '(') (cpsseq e (cpsterminal ')'))) x in
    cpsseq e eof
  in expr (cpsterminal 'n') (of_string "n*(n-n)") (function s -> Printf.printf "Test9::success\n");
     expr (cpsalt (cpsterminal 'a') (cpsterminal 'b')) (of_string "a*(b+a)") (function s -> Printf.printf "Test9.5::success\n");

  end
