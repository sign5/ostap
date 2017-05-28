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
  let table = Hashtbl.create 16

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

  let memo name f =
    function s -> (
      if (Hashtbl.mem table (s, name))
      then begin
        Hashtbl.find table (s, name)
      end
      else begin
        add table (s, name) (memoresult (fun () -> f s));
        Hashtbl.find table (s, name)
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

  let cpsalt name a b =
    memo name
    (function s -> let x = lazy (b s) in
      (function k -> begin a s k; force x k end))

  let rec test1 x = cpsseq (cpsterminal 'a') (cpsterminal 'b') x
  in test1 (of_string "ab") (function s -> Printf.printf "Test1::success\n");
     test1 (of_string "cab") (function s -> Printf.printf "Test1.5::success\n");

  let rec test3 x = cpsalt 1 (cpsseq (cpsterminal 'a') test3) (cpsterminal 'a') x
  in cpsseq test3 eof (of_string "aaa") (function s -> Printf.printf "Test3::success\n");

  let rec test4 x = cpsalt 1 (cpsseq test4 (cpsterminal 'a')) (cpsterminal 'a') x
  in cpsseq test4 eof (of_string "aaa") (function s -> Printf.printf "Test4::success\n");

  let rec test5 x = cpsalt 1 (cpsseq test5 (cpsseq (cpsterminal '+') test5)) (cpsterminal 'a') x
  in cpsseq test5 eof (of_string "a+a+a+a") (function s -> Printf.printf "Test5::success\n");

  let rec test6 x = cpsalt 1 (cpsseq (cpsterminal 'a') (cpsseq test6 (cpsterminal 'a')))
                                     (cpsalt 2 (cpsseq (cpsterminal 'b') (cpsseq test6 (cpsterminal 'b')))
                                               (cpsalt 3 (cpsseq (cpsterminal 'c') (cpsseq test6 (cpsterminal 'c')))
                                                         cpsepsilon)) x
  in cpsseq test6 eof (of_string "acbbca") (function s -> Printf.printf "Test6::success\n");

  let rec test7  x = cpsalt 1 mulli (cpsseq test7 (cpsseq (cpsterminal '+') mulli)) x
  and      mulli x = cpsalt 2 primary (cpsseq mulli (cpsseq (cpsterminal '*') primary)) x
  and    primary x = cpsalt 3 (cpsterminal 'a') (cpsalt 4 (cpsterminal 'b') (cpsterminal 'c')) x
  in cpsseq test7 eof (of_string "a+b+c") (function s -> Printf.printf "Test7::success\n");
     cpsseq test7 eof (of_string "a*b+c") (function s -> Printf.printf "Test7.5::success\n");

  let expr n =
    let rec e s = cpsalt 1 m (cpsseq e (cpsseq (cpsalt 2 (cpsterminal '+') (cpsterminal '-')) m)) s
    and     m s = cpsalt 3 p (cpsseq m (cpsseq (cpsalt 4 (cpsterminal '*') (cpsterminal '/')) p)) s
    and     p s = cpsalt 4 n (cpsseq (cpsterminal '(') (cpsseq e (cpsterminal ')'))) s in
    cpsseq e eof
  in expr (cpsterminal 'n') (of_string "n*(n-n)") (function s -> Printf.printf "Test8::success\n");
     expr (cpsalt 5 (cpsterminal 'a') (cpsterminal 'b')) (of_string "a*(b+a)") (function s -> Printf.printf "Test9::success\n");

  end
