open GT
open List
open Hashtbl
open Printf
open String

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
          let ki = function s -> (if (not (List.mem s !rs))
                                  then begin
                                    rs := s::!rs;
                                    let f elem = elem s in
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

  let cpsterminal c =
    function
    | s::ss when s = c -> success ss
    | ss -> failure ss

  let cpsepsilon = success

  let cpsseq a b s =
    function k ->
      a s (function ss -> b ss k)

  let cpsalt a b =
    memo(
      function s ->
        (function k -> begin a s k; b s k end))

  let rec test1 = cpsseq (cpsterminal 'a') (cpsterminal 'b')
  in test1 (of_string "ab") (function s -> Printf.printf "Test1::success\n");
  let rec test2 = cpsseq (cpsterminal 'a') (cpsterminal 'b')
  in test2 (of_string "caba") (function s -> Printf.printf "Test2::success\n");
  let rec test3 = cpsalt (cpsseq (cpsterminal 'a') (cpsseq test3 (cpsterminal 'a')))
                         (cpsalt (cpsseq (cpsterminal 'b') (cpsseq test3 (cpsterminal 'b')))
                                 (cpsseq (cpsterminal 'c') (cpsseq test3 (cpsterminal 'c'))))
  in test3 (of_string "accbabcca") (function s -> Printf.printf "Test3::success\n");
  end
