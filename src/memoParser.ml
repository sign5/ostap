open Matcher
open Combinators     

class t s = 
  object (this : 'self)
    inherit Matcher.t s
    
    val table : ((int * int) * int) list = []

    method memoize : 'p 'e . ('self, 'p, 'e) parse -> ('self, 'p, 'e) result = 
      fun p -> 
        let getParsedValue t p pos =
          let equal (f0, p0) (f1, p1) = f0 == f1 && p0 = p1 in
          let rec find key tab = 
            match tab with 
            | [] -> raise Not_found
            | (k, v) :: t -> if equal key k 
                             then v
                             else find key t
          in
          Obj.magic (find (Obj.magic p, pos) t)
        in
        let replaceValue t p pos v =
          ((Obj.magic p, pos), (Obj.magic v)) :: t 
        in
        let rec increaseBound t p pos =
          match p {< table = t >} with
          | Failed _ -> getParsedValue t p this#pos            
          | Parsed ((_, s), _) as parsed -> 
            if s#pos > pos 
            then increaseBound (replaceValue t p this#pos parsed) p s#pos
            else getParsedValue t p this#pos
        in
        try 
          getParsedValue table p this#pos           
        with  
          Not_found -> 
            match p {< table = replaceValue table p this#pos (Failed None) >} with
            | Failed _ as r -> r
            | Parsed ((b, s'), e) as r ->
              increaseBound (replaceValue table p this#pos r) p this#pos
  end
