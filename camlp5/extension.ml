(*
 * Extension: a camlp5 extension to wrap Ostap's combinators.
 * Copyright (C) 2006-2009
 * Dmitri Boulytchev, St.Petersburg State University
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file COPYING).
 *)

(** Pa_ostap --- a camlp5 syntax extension for BNF-like grammar definitions. *)

(**

  {2 General description}

  [Pa_ostap] extends Objective Caml grammar with construct [ostap (..)], introduced
  at structure and expression levels. The exact allowed forms are the following:

  {ol
   {- [ostap] {i doc_tag} [(]{i rules}[)] --- at the structure (module implementation) level;}
   {- [let ostap] {i doc_tag} [(]{i rule}[)] --- at the let-binding level; introduces {i recursive} binding;}
   {- [ostap] {i doc_tag} [(]{i rules}[)] --- at the expression level;}
   {- [ostap] [(]{i parse_expr}[)] --- at the expression level;}
   {- [let ostap] {i doc_tag} [(]{i rules}[) in] {i expr} --- at the expression level;}
  }

  In the specification above {i rules} denotes a sequence of grammar rules, {i rule} --- a single
  rule, {i parse_expr} --- parse expression, {i doc_tag} --- documentation specifier. Below the
  examples of all these constructs are given:

  {[
   (* Grammar rules specification at the structure level; rules are mutually recursive *)
   ostap (
     x: IDENT; (* rule defining parser x *)
     y: CONST  (* rule defining parser y *)
   )

   (* Grammar rule at the let-binding level; bindings are mutually recursive *)
   let ostap (x: IDENT) (* rule defining parser x *)
   and ostap (y: CONST) (* rule defining parser y *)
   and u = 3            (* an example to demonstrate interoperability with other let-bindings *)

   let _ =
     (* Let-bindings at expression level; x and y are mutually recursive *)
     let ostap (x: IDENT; y: CONST) in
     (* Grammar expression *)
     let p = ostap (x y) in
     ()
  ]}

  All these constructs are converted into pure OCaml using [Ostap] parser combinators.

  While [Ostap] is purely abstract with regard to stream implementation [Pa_ostap]
  additionally provides convenient integration of parsing and lexing by considering {i streams
  as objects}. Namely, the stream of tokens [L]{_1}, [L]{_2}, ..., [L]{_k} is represented by an object
  with member functions [getL]{_1}, [getL]{_2}, ..., [getL]{_k}. Such a representation allows
  to freely combine various parser functions that operate on different types of streams with almost no
  type limitations at construction time.

  Additionally to this documentation we provide a closed example of how to use [Pa_ostap] (see
  [sample] directory of the distribution).

  {2 Parse expressions}

  The syntax of parse expressions is as follows (text in {b bold} denotes meta-language syntax description
  symbols):

  [parse_expr] {b :} [alternative]{_[1]} {b | } [alternative]{_[2]} {b | ... |} [alternative]{_[k]}

  [alternative] {b :} [prefixed+] {b \[ } [semantic]  {b \] }

  [prefixed] {b : } {b \[ } [-] {b \] } [basic]

  [basic] {b : } {b \[ } [binding] {b \] } [postfix] {b \[ } [predicate] {b \]}

  [postfix] {b : } [primary] {b | } [postfix] {b ( } [*] {b \[} folding {b \]} {b | } [+] {b \[} folding {b \]} {b | } [?] {b | } [:: (] {i EXPR} [)] {b ) }

  [folding] {b : } {b with} {b \{} {i EXPR} {b \}} {b \{} {i EXPR} {b \}}

  [primary] {b : } {i UIDENT} {b | } [parser] {b \[ } [parameters] {b \] } {b | } [string] {b | } [$] {b | ( } [parse_expr] {b )}

  [parser] {b : } {i LIDENT} {b | } [!(]{i EXPR}[)]

  [string] {b : } {i STRING} {b | } [$(]{i EXPR}[)]

  [parameters] {b : } {b (}[\[] {i EXPR} [\]]{b )*}

  [binding] {b : } {i PATT} [:]

  [predicate] {b : } [=> {] {i EXPR}  [}] {b \[} [::(] {i EXPR} [)]{b \]} [=>]

  [semantic] {b : } [{] {i EXPR} [}]

  Here {i UIDENT} and {i LIDENT} stand for identifiers starting from uppercase and lowercase letters
  correspondingly, {i STRING} --- for OCaml strings, {i EXPR} --- for OCaml expression, {i PATT} --- for OCaml pattern.

  [parser] within parse expression denotes a {i parse function} that applied to a stream to
  obtain parsed value and residual stream (see module [Ostap.Combinators]). Each reference is either a {i LIDENT} or
  arbitrary OCaml expression of appropriate type, surrounded by [!(...)]. Parser invocation may be equipped with parameters
  each of which has to be surrounded by [\[...\]] (partial application is allowed as well).
  {i UIDENT} is treated as a lexeme reference;
  thought generally speaking parsing with [Ostap] does not require any lexer to be provided (you may instead supply
  a set of basic parse functions in any way you find convenient) [Pa_ostap] additionally operates with some predefined
  representation of streams as objects (see module [Matcher]). This representation does not interfere with the
  common approach and you need not use this feature unless you explicitly apply to it. There are only three constructs
  that refer to object implementation of streams: {i UIDENT}, [$(]{i EXPR}[)] and {i STRING}. If you use {i UIDENT} in grammar
  expression, for example {i NAME}, then the stream to parse with this expression has to provide a member function
  {i getNAME}. Similarly using {i STRING} in expression requires stream to provide a member {i look}. Finally you
  may match a stream against value of any OCaml expression of string type by surrounding it with [$(...)].

  Postfix operators [+], [*] and [?] denote respectively one-or-more iteration, zero-or-more iteration and
  optional value. Postfix operator [::(]{i EXPR}[)] can be used to {i comment} the reason returned on
  failure with the given reason value (see {!Ostap.Combinators.comment} function and module {!Reason} as reference implementation).

  Additionally some folding can be specified for postfix [+] and [*] operators. The folding has the form
  {b with \{} {i EXPR}{b \}\{} {i EXPR} {b \}} where the first expression in curved brackets denotes
  initial value for folding with function given by the second expression. For example

  {[
    callee:expression call:(-"(" arguments -")")* with{callee}{fun callee args -> `Call (callee, args)} {call}
  ]}

  is equivalent to

  {[
    callee:expression call:(-"(" arguments -")")* {List.fold_left (fun callee args -> `Call (callee, args)) callee call}
  ]}

  Symbol [$] within parse expression serves as a shortcut for {!Ostap.Combinators.lift} and so delivers underlying stream as
  its semantic value.

  Prefix operator [-] is used to {i omit} parsed value from the result of parsing (the parsing of its operand however is
  not omitted).

  Prefix construct {i PATT}[:] is used to match successfully parsed value against pattern {i PATT}; this matching
  may provide bindings that can be used later.

  Construct [=>{]{i EXPR}[}=>] is used to supply additional check of successfully parsed value; {i EXPR} has to
  be of boolean type and may use bindings made before.

  We will not describe the meaning of all constructs in all details since generally it follows the common
  BNF style; instead we demonstrate some examples that cover all cases of their exploration.

  {b Examples:}

  {ol
    {li ["(" expression ")"] is a grammar expression to define a function that matches a stream against successive
     occurrences of ["("], that that parsed by [expression], and [")"]. On success this function returns {i a triple}:
     the token for ["("] (of type determined by stream implementation), the value parsed by [expression], and the token
     for [")"]. There are generally two ways to exclude ["("] and [")"] from the result. The first way is to bind the
     result of [expression] to some name and then explicitly specify the result of grammar expression as follows:

     ["(" e:expression ")" {e}]

     The second is just to say to omit brackets:

     [-"(" expression -")"].

     Note that you may specify arbitrary pattern in the left part of binding. Prefix omitting operator "[-]" may also be
     applied to any grammar expression, enclosed in brackets.
    }
    {li [hd:item tl:(-"," item)* {hd :: tl}] defines a function to parse a list of [item]s.}
    {li [(s:string {`Str s} | x:integer {`Int x})*] defines a function to parse a list of strings or integers.}
    {li [hd:integer tl:(-(","?) integer)* {hd :: tl}] parses a list of integers delimited by optional commas.}
    {li [x:integer => {x > 0}::("positive value expected") => {x}] parses positive integer value.}
    {li [x:(integer?) => {match x with Some 0 -> false | _ -> true} => {x}] parses optional non-zero integer value.}
    {li [x:!(MyParseLibrary.MyModule.parseIt)] parses a stream with parse function specified by qualified name.}
  }

  In all examples above we assume that [integer] parses integer value, [string] --- string value.

  {2 Rules}

  Rule is named and optionally parameterized parse expression; several mutually-recursive rules may be
  defined at once. The syntax of rule definition is

  [rule] {b : } {i LIDENT} [arguments] [:] {b \[} [predicate] {b \]} [parse_expr]

  [rules] {b : } [rule]{_1}; [rule]{_2}; ...; [rule]{_k}

  [arguments] {b : ( }[\[]{i PATT}[\]] {b )*}

  For example,

  {[
   ostap (
     sequence[start]: item[start] | next:item[start] sequence[next];
     item[start]: x:integer {x+start} | ";" {start};
     entry: sequence[0]
   )
  ]}

  declares (among others) the parser function [entry] which parses and sums a semicolon-terminated
  sequence of integers.

  {2 Documentation generation}

  Option [-tex ]{i filename} makes [Pa_ostap] generate [LaTeX] documentation for all rules.
  On default all text is placed into specified file; however the output can be split into
  several files by specifying [doc_tag] option for [ostap] construct. The syntax of option is as
  follows:

  [doc_tag] {b : \[} [\[] {i STRING} [\]] {b \]}

  With this option provided the documentation for corresponding rules will be placed in
  file with name {i filename}[.]{i tagname}[.tex], where {i filename} is the name specified
  by option [-tex] and {i tagname} is string value of [doc_tag].

  Generated documentation uses [ostap.sty] package which provides cross-references and
  automatic layout for most of the cases. To obtain good documentation it is recommended
  to use simple parsers (i.e. identifiers) in grammar rules. Parameterized rules are
  supported as well.
*)

(**/**)

#load "pa_extend.cmo";;
#load "q_MLast.cmo";;

open Pcaml
open Printf
open MLast

open BNF3

module Args =
  struct

    let (h : (string, string) Hashtbl.t) = Hashtbl.create 1024

    let register x = Hashtbl.add h x x
    let wrap     x =
      try Expr.custom [`S (Hashtbl.find h x)] with Not_found -> Expr.nonterm x

    let clear () = Hashtbl.clear h

  end

module Uses =
  struct

    let (h : (string, unit) Hashtbl.t) = Hashtbl.create 1024

    let register x = Hashtbl.add h x ()
    let has      x = Hashtbl.mem h x

    let clear () = Hashtbl.clear h

  end

module Cache =
  struct

    let (h : (string, Expr.t) Hashtbl.t) = Hashtbl.create 1024

    let compress x =
      let b = Buffer.create 1024 in
      let f = ref false in
      for i = 0 to String.length x - 1
      do
          match x.[i] with
	    ' ' -> if !f then () else (Buffer.add_char b ' '; f := true)
	  | '\t' | '\n' -> f := false
	  | c -> Buffer.add_char b c; f := false
      done;
      Buffer.contents b

    let cache x y = Hashtbl.add h (compress x) y

    let rec cached x =
      let x = compress x in
      let rec substitute acc s i j =
	let len = String.length s in
	if j < i then
	  if i < len then substitute acc s (i+1) (len-1) else List.rev (`S s :: acc)
        else if i = len then List.rev (`S s :: acc)
             else
	       let d = String.sub s i (j-i+1) in
	       try
		 substitute
		   (`T (Hashtbl.find h d) :: (`S (String.sub s 0 i) :: acc))
		   (String.sub s (j + 1) (len - j - 1))
		   0
		   (len - j - 2)
	       with
		 Not_found -> substitute acc s i (j-1)
      in
      match substitute [] x 0 (String.length x - 1) with
	[`S s] -> Args.wrap s
      | list   -> Expr.custom list

  end

let printBNF  = ref (fun (_: string option) (_: string) -> ())
let printExpr = ref (fun (_: MLast.expr) -> "")
let printPatt = ref (fun (_: MLast.patt) -> "")

let texDef     def  = Def.toTeX def
let texDefList defs =
  let buf = Buffer.create 1024 in
  List.iter (fun def -> Buffer.add_string buf (sprintf "%s\n" (Def.toTeX def))) defs;
  Buffer.contents buf

let bindOption x f =
  match x with
    None -> None
  | Some x -> Some (f x)

let rec get_ident = function
    <:expr< $lid:x$ >> -> [x]
  | <:expr< [| $list:el$ |] >> -> List.flatten (List.map get_ident el)
  | <:expr< ($list:el$) >> -> List.flatten (List.map get_ident el)
  | <:expr< $p1$ $p2$ >> -> get_ident p1 @ get_ident p2
  | <:expr< { $list:lel$ } >> ->
      List.flatten (List.map (fun (lab, e) -> get_ident e) lel)
  | <:expr< ($e$ : $_$) >> -> get_ident e
  | _ -> []

let rec get_defined_ident = function
    <:patt< $_$ . $_$ >> -> []
  | <:patt< _ >> -> []
  | <:patt< $lid:x$ >> -> [x]
  | <:patt< ($p1$ as $p2$) >> -> get_defined_ident p1 @ get_defined_ident p2
  | <:patt< $int:_$ >> -> []
  | <:patt< $flo:_$ >> -> []
  | <:patt< $str:_$ >> -> []
  | <:patt< $chr:_$ >> -> []
  | <:patt< [| $list:pl$ |] >> -> List.flatten (List.map get_defined_ident pl)
  | <:patt< ($list:pl$) >> -> List.flatten (List.map get_defined_ident pl)
  | <:patt< $uid:_$ >> -> []
  | <:patt< ` $_$ >> -> []
  | <:patt< # $list:_$ >> -> []
  | <:patt< $p1$ $p2$ >> -> get_defined_ident p1 @ get_defined_ident p2
  | <:patt< { $list:lpl$ } >> ->
      List.flatten (List.map (fun (lab, p) -> get_defined_ident p) lpl)
  | <:patt< $p1$ | $p2$ >> -> get_defined_ident p1 @ get_defined_ident p2
  | <:patt< $p1$ .. $p2$ >> -> get_defined_ident p1 @ get_defined_ident p2
  | <:patt< ($p$ : $_$) >> -> get_defined_ident p
  | <:patt< ~{$_$} >> -> []
  | <:patt< ~{$_$ = $p$} >> -> get_defined_ident p
  | <:patt< ?{$_$} >> -> []
  | <:patt< ?{$lid:s$ = $_$} >> -> [s]
  | <:patt< ?{$_$ = ?{$lid:s$ = $e$}} >> -> [s]
  | <:patt< $anti:p$ >> -> get_defined_ident p
  | _ -> []

EXTEND
  GLOBAL: expr patt str_item let_binding;

  doc_name: [ [ "["; name=STRING; "]" -> name ] ];


  str_item: LEVEL "top" [
    [ "ostap"; doc=OPT doc_name; "("; rules=o_rules; ")" ->
      let (numberOfRules, (fixedRuleExprs1, fixedRuleExprs2), defs, namePatts, (gen_fixBodyWithArgs1, gen_fixBodyWithArgs2), paramNums) = rules in
      !printBNF doc (texDefList defs);
      let makePattsAndExprs str num =
        let nameSeq =
          let rec loop n =
            if n > num
            then []
            else (str ^ (string_of_int n)) :: (loop (n + 1))
          in loop 1
        in (List.map (fun name -> <:patt< ($lid:name$)>>) nameSeq,
            List.map (fun name -> <:expr< ($lid:name$)>>) nameSeq)
      in
      let (fakePatts, fakeExprs) = makePattsAndExprs "_nonrecursiveParser" numberOfRules in
      let (fakePatts1, fakeExprs1) = makePattsAndExprs "_generatedParser" numberOfRules in
      let (fakePatts2, fakeExprs2) = makePattsAndExprs "_typeCheckingGeneratedParser" numberOfRules in
      let insideExprWithFixpoint fixedRuleExprs gen_fixBodyWithArgs =
        let rules = List.combine fakePatts fixedRuleExprs in
        let fixPointRule = [(<:patt< $lid:"_generated_fixpoint"$>>, gen_fixBodyWithArgs)] in
        let fix_gen = List.fold_left (fun exprAcc fakeExpr -> <:expr< $exprAcc$ $fakeExpr$ >>) <:expr< _generated_fixpoint >> fakeExprs in
        let insideExpr = <:expr< let $opt:false$ $list:rules$ in $fix_gen$ >> in
        <:expr< let $opt:false$ $list:fixPointRule$ in $insideExpr$ >>
      in
      let fakeTuplePatt1 =
        match fakePatts1 with
        | [ ] -> <:patt< () >>
        | [x] -> x
        |  x  -> <:patt< ( $list:x$ ) >>
      in
      let fakeTuplePatt2 =
        match fakePatts2 with
        | [ ] -> <:patt< () >>
        | [x] -> x
        |  x  -> <:patt< ( $list:x$ ) >>
      in
      let tupleRule = [(fakeTuplePatt1, insideExprWithFixpoint fixedRuleExprs1 gen_fixBodyWithArgs1);
                       (fakeTuplePatt2, insideExprWithFixpoint fixedRuleExprs2 gen_fixBodyWithArgs2)]
      in
      let fakeExprTuple1 =
        match fakeExprs1 with
        | [x] -> let le = [x; List.hd fakeExprs2] in <:expr< ( $list:le$ ) >>
        |  _  -> let x = List.map2 (fun expr1 expr2 -> let le = [expr1; expr2] in <:expr< ( $list:le$ ) >>) fakeExprs1 fakeExprs2 in <:expr< ( $list:x$ ) >>
      in
      (* let fakeExprTuple2 =
        match fakeExprs2 with
        | [x] -> x
        |  x  -> <:expr< ( $list:x$ ) >>
      in *)
      let namePattTuple =
        match namePatts with
        | [x] -> x
        |  x  -> <:patt< ( $list:x$ ) >>
      in
      let outerRules = [(namePattTuple, <:expr< let $opt:false$ $list:tupleRule$ in $fakeExprTuple1$ >>)] in
      <:str_item< value $opt:false$ $list:outerRules$ >>
    ]
  ];

  let_binding: [
    [ "ostap"; doc=OPT doc_name; "("; rule=o_rule; ")" ->
      let (((name, (rule, _)), def), _) = rule in
      !printBNF doc (texDef def);
      (<:patt< $lid:name$ >>, rule)
    ]
  ];

  expr: LEVEL "expr1" [
    [ "ostap"; "("; ((p1, p2), tree)=o_alternatives; ")" ->
      (match tree with Some tree -> Cache.cache (!printExpr <:expr< Ostap.Combinators.Mem.memoize $p1$>>) tree | None -> ());
      let le = [<:expr< Ostap.Combinators.Mem.memoize $p1$ >>; p2] in <:expr< ( $list:le$ ) >>
    ]
  ];

  expr: LEVEL "expr1" [
    [ "let"; "ostap"; doc=OPT doc_name; "("; rules=o_rules; ")"; "in"; e=expr LEVEL "top" ->
      let (numberOfRules, (fixedRuleExprs1, fixedRuleExprs2), defs, namePatts, (gen_fixBodyWithArgs1, gen_fixBodyWithArgs2), paramNums) = rules in
      !printBNF doc (texDefList defs);
      let makePattsAndExprs str num =
        let nameSeq =
          let rec loop n =
            if n > num
            then []
            else (str ^ (string_of_int n)) :: (loop (n + 1))
          in loop 1
        in (List.map (fun name -> <:patt< ($lid:name$)>>) nameSeq,
            List.map (fun name -> <:expr< ($lid:name$)>>) nameSeq)
      in
      let (fakePatts, fakeExprs) = makePattsAndExprs "_nonrecursiveParser" numberOfRules in
      let (fakePatts1, fakeExprs1) = makePattsAndExprs "_generatedParser" numberOfRules in
      let (fakePatts2, fakeExprs2) = makePattsAndExprs "_typeCheckingGeneratedParser" numberOfRules in
      let insideExprWithFixpoint fixedRuleExprs gen_fixBodyWithArgs =
        let rules = List.combine fakePatts fixedRuleExprs in
        let fixPointRule = [(<:patt< $lid:"_generated_fixpoint"$>>, gen_fixBodyWithArgs)] in
        let fix_gen = List.fold_left (fun exprAcc fakeExpr -> <:expr< $exprAcc$ $fakeExpr$ >>) <:expr< _generated_fixpoint >> fakeExprs in
        let insideExpr = <:expr< let $opt:false$ $list:rules$ in $fix_gen$ >> in
        <:expr< let $opt:false$ $list:fixPointRule$ in $insideExpr$ >>
      in
      let fakeTuplePatt1 =
        match fakePatts1 with
        | [ ] -> <:patt< () >>
        | [x] -> x
        |  x  -> <:patt< ( $list:x$ ) >>
      in
      let fakeTuplePatt2 =
        match fakePatts2 with
        | [ ] -> <:patt< () >>
        | [x] -> x
        |  x  -> <:patt< ( $list:x$ ) >>
      in
      let tupleRule = [(fakeTuplePatt1, insideExprWithFixpoint fixedRuleExprs1 gen_fixBodyWithArgs1);
                       (fakeTuplePatt2, insideExprWithFixpoint fixedRuleExprs2 gen_fixBodyWithArgs2)]
      in
      let fakeExprTuple1 =
        match fakeExprs1 with
        | [x] -> let le = [x; List.hd fakeExprs2] in <:expr< ( $list:le$ ) >>
        |  _  -> let x = List.map2 (fun expr1 expr2 -> let le = [expr1; expr2] in <:expr< ( $list:le$ ) >>) fakeExprs1 fakeExprs2 in <:expr< ( $list:x$ ) >>
      in
      (* let fakeExprTuple2 =
        match fakeExprs2 with
        | [x] -> x
        |  x  -> <:expr< ( $list:x$ ) >>
      in *)
      let namePattTuple =
        match namePatts with
        | [x] -> x
        |  x  -> <:patt< ( $list:x$ ) >>
      in
      let outerRules = [(namePattTuple, <:expr< let $opt:false$ $list:tupleRule$ in $fakeExprTuple1$ >>)] in
      <:expr< let $opt:false$ $list:outerRules$ in $e$ >>
     ]
  ];

  o_rules: [
    [ rules=LIST1 o_rule SEP ";" ->
      let (rules, paramNums) = List.split rules in
      let (rules, defs) = List.split rules in
      let (names, rules) = List.split rules in
      let (ruleExprs1, ruleExprs2) = List.split rules in
      let namePatts = List.map (fun name -> <:patt< $lid:name$>>) names in
      let makePattsAndExprs str num =
        let nameSeq =
          let rec loop n =
            if n > num
            then []
            else (str ^ (string_of_int n)) :: (loop (n + 1))
          in loop 1
        in (List.map (fun name -> <:patt< ($lid:name$)>>) nameSeq,
            List.map (fun name -> <:expr< ($lid:name$)>>) nameSeq)
      in
      let (fakePatts, fakeExprs) = makePattsAndExprs "_nonrecursiveParser" (List.length names) in
      let (fixArgPatts, fixArgExprs) = makePattsAndExprs "_f" (List.length names) in
      let (lazyPatts, lazyExprs) = makePattsAndExprs "_p" (List.length names)in
      let forcedLazyExprs = List.map (fun lazyExpr -> <:expr< (Lazy.force_val $lazyExpr$) >>) lazyExprs in
      let lazyRules = List.map2 (fun fixArgExpr paramNum ->
          let (paramPatts, paramExprs) = makePattsAndExprs "_param" paramNum in
          let (paramPatts', paramExprs') = makePattsAndExprs "_param'" paramNum in
          let cmp = List.fold_right2
                      (fun paramExpr paramExpr' acc ->
                         let tryExpr =
                           let pwel = [(<:patt< Invalid_argument _ >>), Ploc.VaVal None, <:expr< $paramExpr$ == $paramExpr'$>>] in
                           <:expr< try $paramExpr$ = $paramExpr'$ with [ $list:pwel$ ]>>
                         in
                         <:expr< $acc$ && ($tryExpr$)>>
                       )
                       paramExprs paramExprs' <:expr< True>> in
          let innerMatch =
            let e = <:expr< $lid:"acc"$>> in
            let pwel = [(<:patt< Some _>>, Ploc.VaVal None,     <:expr< $lid:"acc"$>>    );
                        (<:patt< None>>,   Ploc.VaVal Some cmp, <:expr< Some $lid:"p'"$>>);
                        (<:patt< _>>,      Ploc.VaVal None,     <:expr< None>>           )] in
            <:expr< match $e$ with [ $list:pwel$ ] >>
          in
          let folder =
            let pwel1 = [(<:patt< $lid:"acc"$>>, Ploc.VaVal None, innerMatch)] in
            let pwel2 = [(<:patt< $lid:"p'"$>>,  Ploc.VaVal None, <:expr< fun [$list:pwel1$]>>)] in
            let pwel3 =
              let paramPatts'' =
                match paramPatts' with
                | [ ] -> <:patt< () >>
                | [x] -> x
                |  x  -> <:patt< ( $list:x$ ) >>
              in
              [(paramPatts'', Ploc.VaVal None, <:expr< fun [$list:pwel2$]>>)]
            in
            <:expr< (fun [$list:pwel3$])>>
          in
          let hashFold = <:expr< (Hashtbl.fold $folder$ $lid:"_table"$ None)>> in
          let fixArgExpr' = List.fold_left (fun exprAcc forcedLazyExpr -> <:expr< $exprAcc$ ($forcedLazyExpr$)>>) fixArgExpr forcedLazyExprs in
          let fixArgExpr1 = List.fold_left (fun exprAcc paramExpr -> <:expr< (Ostap.Combinators.Mem.mapply $exprAcc$ $paramExpr$)>>) fixArgExpr' paramExprs in
          let fixArgExpr2 = List.fold_left (fun exprAcc paramExpr -> <:expr< ($exprAcc$ $paramExpr$)>>) fixArgExpr' paramExprs in
          let memoBody =
            let r = <:expr< $lid:"_r"$>> in
            let el =
              let paramExprs'' =
                match paramExprs with
                | [ ] -> <:expr< () >>
                | [x] -> x
                |  x  -> <:expr< ( $list:x$ ) >>
              in
              [<:expr< Hashtbl.add $lid:"_table"$ $paramExprs''$ $r$>>; <:expr< $r$ $lid:"_s"$>>] in
            <:expr< do {$list:el$}>>
          in
          let outerMatch fixArgExpr =
            let rules = [(<:patt< $lid:"_r"$>>, fixArgExpr)] in
            let pwel = [(<:patt< None>>,           Ploc.VaVal None, <:expr< let $opt:false$ $list:rules$ in $memoBody$>>);
                        (<:patt< Some $lid:"x"$>>, Ploc.VaVal None, <:expr< $lid:"x"$ $lid:"_s"$>>)] in
            <:expr< match $hashFold$ with [ $list:pwel$ ]>> in
          let memoizedRule1 = List.fold_right (
            fun paramPatt exprAcc ->
              let pwel = [(paramPatt, Ploc.VaVal None, exprAcc)] in
              <:expr< Ostap.Combinators.Mem.memoize (fun [$list:pwel$]) >>
            ) (paramPatts @ [<:patt< $lid:"_s"$>>]) (outerMatch fixArgExpr1) in
          let memoizedRule2 = List.fold_right (
            fun paramPatt exprAcc ->
              let pwel = [(paramPatt, Ploc.VaVal None, exprAcc)] in
              <:expr< fun [$list:pwel$] >>
            ) (paramPatts @ [<:patt< $lid:"_s"$>>]) (outerMatch fixArgExpr2) in
          let fixArgExpr1 =
            let rules = [(<:patt< $lid:"_table"$>>), <:expr< Hashtbl.create $int:"16"$>>] in
            <:expr< let $opt:false$ $list:rules$ in $memoizedRule1$ >>
          in
          let fixArgExpr2 =
            let rules = [(<:patt< $lid:"_table"$>>), <:expr< Hashtbl.create $int:"16"$>>] in
            <:expr< let $opt:false$ $list:rules$ in $memoizedRule2$ >>
          in
          <:expr< lazy ($fixArgExpr1$) >>, <:expr< lazy ($fixArgExpr2$) >>
      ) fixArgExprs paramNums in
      let lazyRules1, lazyRules2 = List.split lazyRules in
      let lazyRules1 = List.combine lazyPatts lazyRules1 in
      let lazyRules2 = List.combine lazyPatts lazyRules2 in
      let finalExpr =
        match forcedLazyExprs with
        | [ ] -> <:expr< () >>
        | [x] -> x
        |  x  -> <:expr< ( $list:x$ ) >>
      in
      let gen_fixBody lazyRules = <:expr< let $opt:true$ $list:lazyRules$ in $finalExpr$ >> in
      let gen_fixBodyWithArgs lazyRules = List.fold_right (
        fun fixArgPatt exprAcc ->
          let pwel = [(fixArgPatt, Ploc.VaVal None, exprAcc)] in
          <:expr< fun [$list:pwel$] >>
        ) fixArgPatts (gen_fixBody lazyRules) in
      let fixedRuleExprs ruleExprs =
        let makeFixed rule =
          List.fold_right (
            fun namePatt exprAcc ->
              let pwel = [(namePatt, Ploc.VaVal None, exprAcc)] in
              <:expr< fun [$list:pwel$] >>
          ) namePatts rule in
        (List.map makeFixed ruleExprs)
      in
      (List.length names, ((fixedRuleExprs ruleExprs1), fixedRuleExprs ruleExprs2), defs, namePatts, ((gen_fixBodyWithArgs lazyRules1), gen_fixBodyWithArgs lazyRules2), paramNums)
    ]
  ];

  o_rule: [
      [ name=LIDENT; args=OPT o_formal_parameters; ":"; ((p1, p2), tree)=o_alternatives ->
        let args' =
          match args with
            None   -> []
          | Some l -> l
        in
        let (rule1, rule2) =
  	      List.fold_right
  	        (fun x (f1, f2) ->
  	          let pwel1 = [(x, Ploc.VaVal None, f1)] in
              let pwel2 = [(x, Ploc.VaVal None, f2)] in
  	          <:expr< Ostap.Combinators.Mem.memoize (fun [$list:pwel1$]) >>, <:expr< (fun [$list:pwel2$]) >>
  	        )
            args'
            (<:expr< $p1$>>, <:expr< $p2$>>)
        in
        let p =
          match args with
            None      -> []
          | Some args ->
  	        let args =
              List.filter
              (fun p ->
                let idents = get_defined_ident p in
                List.fold_left (fun acc ident -> acc || (Uses.has ident)) false idents
              )
              args
  	        in
            List.map !printPatt args
        in
        let tree =
          match tree with
            None      -> Expr.string ""
          | Some tree -> tree
        in
        let def =
          match p with
            []   -> Def.make  name tree
          | args -> Def.makeP name args tree
        in
        Args.clear ();
        Uses.clear ();
        ((name, (rule1, rule2)), def), List.length args'
      ]
  ];

  o_formal_parameters: [ [ p=LIST1 o_formal_parameter -> p ]];

  o_formal_parameter: [
    [ "["; p=patt; "]" ->
        List.iter Args.register (get_defined_ident p);
        p
    ]
  ];

  o_alternatives: [
    [ p=LIST1 o_alternativeItem SEP "|" ->
      match p with
        [p] -> p
      |  _  ->
        let (p, trees) = List.split p in
        let trees =
          List.map
          (fun x -> match x with Some x -> x)
          (List.filter (fun x -> x <> None) trees)
        in
          match
            List.fold_right
              (fun (item1, item2) expr ->
                match expr with
                  None -> Some (item1, item2)
                | Some (expr1, expr2) -> Some (<:expr< Ostap.Combinators.alt $item1$ $expr1$ >>, <:expr< Ostap.Combinators.alt $item2$ $expr2$ >>)
              ) p None
          with
            None        -> raise (Failure "internal error --- must not happen")
          | Some (x, y) -> ((x, y), match trees with [] -> None | _ -> Some (Expr.alt trees))
    ]
  ];

  o_alternativeItem: [
    [ g=OPT o_guard; p=LIST1 o_prefix; s=OPT o_semantic ->
      let (p, trees) = List.split p in
      let trees =
        List.map
          (fun x -> match x with Some x -> x)
          (List.filter (fun x -> x <> None) trees)
      in
      let trees =
        match trees with
          [] -> None
        | _  -> Some (Expr.seq trees)
      in
      let (s, isSema) =
        match s with
          Some s -> (s, true)
        | None ->
          let (tuple, _) =
            List.fold_right
              (fun (_, omit, _, (_, _)) ((acc, i) as x) ->
                if omit then x else (<:expr< $lid:"_" ^ (string_of_int i)$>> :: acc, i+1)
              )
              p
              ([], 0)
          in
          match tuple with
            []  -> (<:expr< () >>, true)
          | [x] -> (x, false)
          |  _  -> (<:expr< ($list:tuple$) >>, true)
      in
      match List.fold_right
        (fun (flag, omit, binding, (p1, p2)) rightPart ->
          let (p1, p2) =
             match flag with
               None -> (p1, p2)
             | Some (f, r) ->
               let pwel =
                 match binding with
                   None   -> [(<:patt< _ >>,   Ploc.VaVal None, f)]
                 | Some p -> [(<:patt< $p$ >>, Ploc.VaVal None, f)]
		           in
		             let pfun = <:expr< fun [$list:pwel$] >> in
		             match r with
		               None ->
                     let t p = <:expr< Ostap.Combinators.guard $p$ $pfun$ None >> in
                     ((t p1), t p2)
		             | Some r ->
			             let pwel =
			               match binding with
			                 None   -> [(<:patt< _ >>,   Ploc.VaVal None, r)]
			               | Some p -> [(<:patt< $p$ >>, Ploc.VaVal None, r)]
                   in
                     let rfun = <:expr< fun [$list:pwel$] >> in
                     let t p = <:expr< Ostap.Combinators.guard $p$ $pfun$ (Some $rfun$) >> in
                     ((t p1), t p2)
          in
          let (n, (right1, right2), combi, isMap) =
            match rightPart with
              None -> (0, (s, s), (fun x y -> <:expr< Ostap.Combinators.map $y$ $x$>>), true)
            | Some (right, n) -> (n, right, (fun x y -> <:expr< Ostap.Combinators.seq $x$ $y$>>), false)
          in
          if not isSema && not omit && isMap && binding = None
          then Some ((p1, p2), n+1)
          else
            let patt = match binding with None -> <:patt< _ >> | Some patt -> patt in
            let (patt, n) = if not omit then (<:patt< ($patt$ as $lid:"_" ^ (string_of_int n)$) >>, n+1) else (patt, n) in
            let pwel1     = [(patt, Ploc.VaVal None, right1)] in
            let pwel2     = [(patt, Ploc.VaVal None, right2)] in
            let sfun1     = <:expr< fun [$list:pwel1$] >> in
            let sfun2     = <:expr< fun [$list:pwel2$] >> in
            Some (((combi p1 sfun1), combi p2 sfun2), n)
         ) p None
      with
        Some ((expr1, expr2), _) ->
        (match g with
           None   -> ((expr1, expr2), trees)
         | Some (g, None) ->
           let t expr = <:expr< Ostap.Combinators.seq (Ostap.Combinators.guard Ostap.Combinators.empty' (fun _ -> $g$) None) (fun _ -> $expr$) >> in
           ((t expr1), t expr2), trees
         | Some (g, Some r) ->
           let t expr = <:expr< Ostap.Combinators.seq (Ostap.Combinators.guard Ostap.Combinators.empty' (fun _ -> $g$) (Some (fun _ -> $r$))) (fun _ -> $expr$) >> in
           ((t expr1), t expr2), trees
        )
      | None -> raise (Failure "internal error: empty list must not be eaten")
    ]
  ];

  o_prefix: [
    [ "%"; s=STRING ->
      let name   = <:expr< $str:s$ >> in
      let regexp = <:expr< $name$ ^ "\\\\\\\\b" >> in
      let look   = <:expr< _ostap_stream # regexp ($name$) ($regexp$) >> in
      let resType' = <:ctyp< $uid:"Types"$ . $lid:"result"$ >> in
      let strType = <:ctyp< $uid:"String"$ . $lid:"t"$ >> in
      let resType = <:ctyp< $resType'$ '$"self"$ '$"b"$ '$"c"$ >> in
      let contType = <:ctyp< '$"alook"$ -> '$"self"$ -> $resType$ >> in
      let methodType = <:ctyp< ! $list:["b"]$ . $strType$ -> $strType$ -> $contType$ -> $resType$ >> in
      let fl = [("regexp", methodType)] in
      let classType = <:ctyp< < $list:fl$ .. > as '$"self"$>> in
      let pwel   = [(<:patt< ( $lid:"_ostap_stream"$ : $classType$ ) >>, Ploc.VaVal None, look)] in
      let e1 = <:expr<fun [$list:pwel$]>> in
      let e2 = <:expr<fun [$list:pwel$]>> in
      let s  = Some (Expr.string (!printExpr name)) in
      ((None, true, None, (e1, e2)), s)
    ] |
    [ m=OPT "-"; (p, s)=o_basic ->
       let (binding, (p1, p2), f) = p in
       ((f, (m <> None), binding, (p1, p2)), s)
    ]
  ];

  o_basic: [
    [ p=OPT o_binding; ((e1, e2), s)=o_postfix; f=OPT o_predicate -> ((p, (e1, e2), f), s) ]
  ];

  o_postfix: [
    [ o_primary ] |
    [ ((e1, e2), s)=o_postfix; "*"; folding=OPT o_folding ->
      let post e =
        match folding with
        | None                -> <:expr< Ostap.Combinators.many     $e$ >>
        | Some (init, folder) -> <:expr< Ostap.Combinators.manyFold $folder$ $init$ $e$ >>
      in
      ((post e1), post e2), bindOption s (fun s -> Expr.star s)
    ] |
    [ ((e1, e2), s)=o_postfix; "+"; folding=OPT o_folding ->
      let post e =
        match folding with
         | None -> <:expr< Ostap.Combinators.some $e$ >>
         | Some (init, folder) -> <:expr< Ostap.Combinators.someFold $folder$ $init$ $e$ >>
      in
      ((post e1), post e2), bindOption s (fun s -> Expr.plus s)
    ] |
    [ ((e1, e2), s)=o_postfix; "?" ->
      let post e = <:expr< Ostap.Combinators.opt $e$ >> in
      ((post e1), post e2), bindOption s (fun s -> Expr.opt s)
    ] |
    [ ((e1, e2), s)=o_postfix; "::"; "("; c=expr; ")" ->
      let post e = <:expr< Ostap.Combinators.comment $e$ ($c$) >> in
      ((post e1), post e2), s
    ]
  ];

  o_folding: [
    [ "with"; "{"; init=expr; "}"; "{"; folder=expr; "}" -> (init, folder)
    ]
  ];

  o_primary: [
    [ (p, s)=o_reference; args=OPT o_parameters ->
          match args with
             None           -> ((<:expr< (Ostap.Combinators.Mem.mapply (fst $p$)) >>, <:expr< snd $p$ >>), Some s)
           | Some (args, a) ->
	       let args = args in
	       let body1 = List.fold_left (fun expr arg -> <:expr< (Ostap.Combinators.Mem.mapply $expr$ $arg$) >>) <:expr< (fst $p$) >> args in
         let body2 = List.fold_left (fun expr arg -> <:expr< $expr$ $arg$ >>) <:expr< (snd $p$) >> args in
	       ((body1, body2), (Some (Expr.apply s a)))
    ] |
    [ p=UIDENT ->
            let p' = "get" ^ p in
            let look = <:expr< _ostap_stream # $p'$ >> in
            let resType' = <:ctyp< $uid:"Types"$ . $lid:"result"$ >> in
            let resType = <:ctyp< $resType'$ '$"self"$ '$"b"$ '$"c"$ >> in
            let contType = <:ctyp< '$"a" ^ p$ -> '$"self"$ -> $resType$ >> in
            let methodType = <:ctyp< ! $list:["b"]$ . $contType$ -> $resType$ >> in
            let fl = [(p', methodType)] in
            let classType = <:ctyp< < $list:fl$ .. > as '$"self"$>> in
            let pwel = [(<:patt< ( $lid:"_ostap_stream"$ : $classType$ ) >>, Ploc.VaVal None, look)] in
            let t = <:expr<fun [$list:pwel$]>> in
            ((t, t), Some (Expr.term p))
    ] |
    [ p=STRING ->
          let look = <:expr< _ostap_stream # look $str:p$ >> in
          let resType' = <:ctyp< $uid:"Types"$ . $lid:"result"$ >> in
          let strType = <:ctyp< $uid:"String"$ . $lid:"t"$ >> in
          let resType = <:ctyp< $resType'$ '$"self"$ '$"b"$ '$"c"$ >> in
          let contType = <:ctyp< '$"alook"$ -> '$"self"$ -> $resType$ >> in
          let methodType = <:ctyp< ! $list:["b"]$ . $strType$ -> $contType$ -> $resType$ >> in
          let fl = [("look", methodType)] in
          let classType = <:ctyp< < $list:fl$ .. > as '$"self"$>> in
          let pwel = [(<:patt< ( $lid:"_ostap_stream"$ : $classType$ ) >>, Ploc.VaVal None, look)] in
          let t = <:expr<fun [$list:pwel$]>> in
          ((t, t), Some (Expr.string p))
    ] |
    [ "$"; "("; p=expr; ")" ->
          let look = <:expr< _ostap_stream # look ($p$) >> in
          let resType' = <:ctyp< $uid:"Types"$ . $lid:"result"$ >> in
          let strType = <:ctyp< $uid:"String"$ . $lid:"t"$ >> in
          let resType = <:ctyp< $resType'$ '$"self"$ '$"b"$ '$"c"$ >> in
          let contType = <:ctyp< '$"alook"$ -> '$"self"$ -> $resType$ >> in
          let methodType = <:ctyp< ! $list:["b"]$ . $strType$ -> $contType$ -> $resType$ >> in
          let fl = [("look", methodType)] in
          let classType = <:ctyp< < $list:fl$ .. > as '$"self"$>> in
          let pwel = [(<:patt< ( $lid:"_ostap_stream"$ : $classType$ ) >>, Ploc.VaVal None, look)] in
          let t = <:expr<fun [$list:pwel$]>> in
          ((t, t), Some (Expr.string (!printExpr p)))
    ] |
    [ "@"; "("; p=expr; n=OPT o_regexp_name; ")" ->
          let name = match n with None -> p | Some p -> p in
          let look = <:expr< _ostap_stream # regexp ($name$) ($p$) >> in
          let resType' = <:ctyp< $uid:"Types"$ . $lid:"result"$ >> in
          let strType = <:ctyp< $uid:"String"$ . $lid:"t"$ >> in
          let resType = <:ctyp< $resType'$ '$"self"$ '$"b"$ '$"c"$ >> in
          let contType = <:ctyp< '$"alook"$ -> '$"self"$ -> $resType$ >> in
          let methodType = <:ctyp< ! $list:["b"]$ . $strType$ -> $strType$ -> $contType$ -> $resType$ >> in
          let fl = [("regexp", methodType)] in
          let classType = <:ctyp< < $list:fl$ .. > as '$"self"$>> in
          let pwel = [(<:patt< ( $lid:"_ostap_stream"$ : $classType$ ) >>, Ploc.VaVal None, look)] in
          let t = <:expr<fun [$list:pwel$]>> in
          ((t, t), Some (Expr.string (!printExpr p)))
    ] |
    [ "$" -> let t = <:expr< Ostap.Combinators.lift >> in ((t, t), None) ] |
    [ "("; ((p1, p2), s)=o_alternatives; ")" -> ((p1, p2), bindOption s (fun s -> Expr.group s)) ]
  ];

  o_regexp_name: [[ ":"; e=expr -> e ]];

  o_reference: [
    [ p=LIDENT -> Uses.register p; (<:expr< $lid:p$ >>, Args.wrap p) ] |
    [ "!"; "("; e=expr; ")" -> (e, Expr.string (!printExpr e)) ]
  ];

  o_parameters: [ [ p=LIST1 o_parameter -> List.split p ]];

  o_parameter: [
    [ "["; e=expr; "]" ->
      List.iter Uses.register (get_ident e);
      (e, Cache.cached (!printExpr e))
    ]
  ];

  o_binding: [
    [ "<"; p=patt; ">"; ":" -> p ] |
    [ p=LIDENT; ":" -> <:patt< $lid:p$ >> ]
  ];

  o_semantic: [ ["{"; e=expr; "}" -> e ] ];

  o_predicate: [ [ "=>"; e=o_guard -> e ] ];

  o_guard: [ [ "{"; e=expr; "}"; r=OPT o_reason; "=>" -> (e, r) ] ];

  o_reason: [ [ "::"; "("; e=expr; ")" -> e ] ];

END;

add_option "-tex"  (Arg.String
		      (fun s ->
		  	   let p = !printBNF in

			   let ouch = open_out (s ^ ".tex") in

			   close_out ouch;

                           printExpr := (fun e -> Eprinter.apply pr_expr Pprintf.empty_pc e);
			   printPatt := (fun p -> Eprinter.apply pr_patt Pprintf.empty_pc p);

			   printBNF  :=
			     (fun name bnf ->
                                  let ouch =
				    match name with
				      None      -> open_out_gen [Open_append; Open_text] 0o66 (s ^ ".tex")
				    | Some name -> open_out (s ^ "." ^ name ^ ".tex")
				  in
			          fprintf ouch "%s" bnf;
			          close_out ouch;
			          p name bnf
			     )
		      )
		   )
           "<name> - print TeX grammar documentation to given file";
