open Lexy
let a = lexTest (readSans "test.sans")
;;

(* print_lex_list a;; *)

let getLContent {label;content;line} = content
let getLLabel {label;content;line} = label
let getLLine {label;content;line} = line

type parseUnit = {
  label : string;
  child : parseUnit list;
  str: string;
  line: int
}
;;

let getPLabel {label;child;str} = label
let getPChild {label;child;str} = child
let getPStr {label;child;str} = str
let getPLine {label;child;str;line} = line
let drop l = 
  match l with
  | [] -> ([],[])
  | x::xs -> ([x],xs)

let twoOprWord = ["ASSIGN";"EQU";"PLUS";"MINUS";"STAR";
"DIVI";"BTH";"LTH";"BRTH";"LETH";"PLUSE";"MINUSE";"MUTIE";
"DIVIE";]  

let parseLex ll =
  let rec processTill ill re el = 
    let processNext xs la st ell rre line=
      let (fst,snd) = (processTill xs [] ell) in
        processTill fst ({label=la;child=(List.rev snd);str=st;line}::rre) el
    in
    let processNextTwo xs la st ell rre line=
      let (fst,snd) = (processTill xs [] ell) in
      let (fstt,one) = (List.tl fst, List.hd fst)in
        processTill fstt ({label=la;child=(List.rev snd);str=st;line}::rre) el
    in
    let processTwo xs la st ell rre line=
      let (fst,snd) = (processTill xs [] ell) in
        processTill fst ({label=la;child=(List.hd rre)::(List.rev snd);str=st;line}::(List.tl rre)) el
      in
    match ill with 
    | [] -> ([], re)
    | x::xs -> 
      if isInStrList (getLLabel x) el then ((x::xs), re)
      else match getLLabel x with
      | "IF" ->  
      processNext xs "IFEXPR" "if" ["ENDIF";"ELSE"] re (getLLine x)
      | "WHILE" -> 
      processNext xs "WEXPR" "while" ["DONE";] re (getLLine x)
      | "TYPE" -> 
      processNext xs "DECL" (getLContent x) ["SEMI";] re (getLLine x)
      | "LPAR" ->
      processNextTwo xs "PAR" "()" (List.concat [["RPAR";];el]) re (getLLine x)
      | "THEN" -> 
      processNext xs "THEN" "then" el re (getLLine x)
      | "DO" -> 
      processNext xs "DO" "do" el re (getLLine x)
      | "ELSE" -> 
      processNext xs "ELSE" "else" el re (getLLine x)
      | "PP" -> 
      processNext xs "PP" "++" el re (getLLine x)
      | "SEMI" ->
      processTill xs re el
      | "DONE" ->
      processTill xs re el
      | "ENDIF" ->
      processTill xs re el
      | "FLOAT" ->
      processTill xs ({label=(getLLabel x);child=[];str=Int32.to_string (Int32.bits_of_float (float_of_string (getLContent x)));line=(getLLine x)}::re) el
      | _ ->
      if isInStrList (getLLabel x) twoOprWord then
        processTwo xs (getLLabel x) (getLContent x) (List.concat [["SEMI";"DO";"THEN";];el]) re (getLLine x) 
      else
      processTill xs ({label=(getLLabel x);child=[];str=(getLContent x);line=(getLLine x)}::re) el
    in
  let rec processToEnd ill =
    let (a,b) = processTill ill [] [] in
    if List.length a <> 0 then 
      List.concat [(List.rev b); (processToEnd a)]
    else List.rev b
  in
  [{label="ST";child=processToEnd ll;str="start";line=0}]

let rec print_parse_list pl n=
  let print_parse p =
    match getPChild p with
    | [] -> Printf.printf "%s|--<%s>:%s|%d\n" n (getPLabel p) (getPStr p) (getPLine p);
    | x -> Printf.printf "%s|-<%s>:%s|%d\n" n (getPLabel p) (getPStr p) (getPLine p);print_parse_list x (n^"|  ")
  in
  match pl with
  | [] -> print_string ""
  | x::xs -> print_parse x;print_parse_list xs n
;;

print_parse_list (parseLex a) "";; 
