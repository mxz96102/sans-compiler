let readSans filename = 
  let lines = ref "" in
  let chan = open_in filename in
  try
    while true; do
      lines := !lines ^ "\n" ^ (input_line chan)
    done; ""
  with End_of_file ->
    close_in chan;!lines^" "
;;
type lexUnit = { label: string; content: string;line: int}

let isNewLine c = c = '\n'
let rec isInStrList str sl=
  match sl with 
  | [] -> false
  | [x] -> if String.equal str x then true else false    
  | x::xs -> if String.equal str x then true else isInStrList str xs
let rec isInList c cl =
  match cl with 
  | [] -> false
  | x::xs -> if c = x then true else isInList c xs
;;
let isNum c = c >= '0' && c <= '9'
let isLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let rec string_of_char_list l n = 
  match l with
  | [] -> n
  | x::xs -> string_of_char_list xs n ^ (Char.escaped x) 

let error = Printf.printf;;

let reWordList = [
  ("if", {label = "IF";content="if";line=0});
  ("endif", {label = "ENDIF";content="endif";line=0});
  ("else", {label = "ELSE";content="else";line=0});
  ("then", {label = "THEN";content="then";line=0});
  ("end", {label = "END";content="end";line=0});
  ("while", {label = "WHILE";content="while";line=0});
  ("do", {label = "DO";content="do";line=0});
  ("done", {label = "DONE";content="done";line=0});  
  ("int", {label = "TYPE";content="int";line=0});
  ("char", {label = "TYPE";content="char";line=0});
  ("float", {label = "TYPE";content="float";line=0});
  ("string", {label = "TYPE";content="string";line=0});  
  ("import", {label = "IMPORT";content="float";line=0}); 
]
let reWords = List.map (fun (a,b) -> a) reWordList
let labelPair {label=la1} {label=la2} = (la1,la2) 
let getLabel {label} = label 
let getContent {content} = content 
let getLine {line} = line
let setLine {label;content;line} l = {label;content;line=l}
let dob a b = b

let rec matchReWord str rl line= 
  match rl with
  | [] -> error "reserve word not found";{label = "NF";content="not found";line=0}
  | x::xs -> let (a,b) = x in 
    if String.equal a str then (setLine b line) else matchReWord str xs line



let rec matchTwo ll re=
  match ll with
  | [] -> re
  | x::[] ->  matchTwo [] (x::re)  
  | x1::x2::xs ->
    match (labelPair x1 x2) with
    | ("BTH", "EQU") -> matchTwo xs ({label="BETH";content=">=";line=(getLine x1)}::re)
    | ("LTH", "EQU") -> matchTwo xs ({label="LETH";content="<=";line=(getLine x1)}::re)
    | ("PLUS", "EQU") -> matchTwo xs ({label="PLUSE";content="+=";line=(getLine x1)}::re)
    | ("MINUS", "EQU") -> matchTwo xs ({label="MINUSE";content="-=";line=(getLine x1)}::re)
    | ("STAT", "EQU") -> matchTwo xs ({label="MUTIE";content="*=";line=(getLine x1)}::re)
    | ("DIVI", "EQU") -> matchTwo xs ({label="DIVI";content="/=";line=(getLine x1)}::re)
    | ("PLUS", "PLUS") -> matchTwo xs ({label="PP";content="++";line=(getLine x1)}::re)  
    | ("MINUS", "MINUS") -> matchTwo xs ({label="MM";content="--";line=(getLine x1)}::re)
    | ("LTH", "MINUS") -> matchTwo xs ({label="ASSIGN";content="<-";line=(getLine x1)}::re)
    | ("INT","VAR") -> 
    dob (Printf.printf "error num %s line num %d\n" (getContent x1) (getLine x1)) (matchTwo [] re)   
    | ("VAR","INT") -> 
    dob (Printf.printf "error num %s line num %d\n" (getContent x1) (getLine x1)) (matchTwo [] re)   
    | ("VAR","VAR") -> 
    dob (Printf.printf "error var %s line num %d\n" (getContent x1) (getLine x1)) (matchTwo [] re)   
    | ("SQUT", _) ->
      let next = (List.hd xs) in
      if String.equal (getLabel next) "SQUT" && String.length (getContent x2) = 1 then
        matchTwo (List.tl xs) ({label="CHAR";content="\'"^(getContent x2)^"\'";line=(getLine x2)}::re)
      else dob (print_string ((getContent x2)^" is not a char\n")) (matchTwo [] re)
    | ("DOT","INT") -> matchTwo xs ({label="FLOAT";content="0."^(getContent x2);line=(getLine x1)}::re)
    | ("INT","DOT") -> 
      let next = (List.hd xs) in
      if String.equal (getLabel next) "INT" then
        matchTwo (List.tl xs) ({label="FLOAT";content=(getContent x1)^"."^(getContent next);line=(getLine x1)}::re)
      else
        matchTwo xs ({label="FLOAT";content=(getContent x1)^".0";line=(getLine x1)}::re)
      | _ -> matchTwo (x2::xs) (x1::re)

let lexTest str = 
  let len = String.length str - 1 in
  let lineNum = ref 0 in
  let rec handleNum n l = 
    let now = str.[n] in
    if isNum now then 
      handleNum (n+1) (now::l)
    else (n - 1, string_of_char_list l "")
    in
  let rec handleName n l = 
    if n > len then (n, (string_of_char_list l "")) else
    let now = str.[n] in
    if isNum now || isLetter now then 
      handleName (n+1) (now::l)
    else (n - 1, (string_of_char_list l ""))
    in
  let rec handleString n l =  
    if n > len then (n, (string_of_char_list l "")) else
    if str.[n] = '\"' then
    (n , (string_of_char_list l "")) 
  else handleString (n+1) (str.[n]::l)
  in
  let rec handle n l=
    if n < len then 
      let now = str.[n] in
      match now with 
      | ' ' -> handle (n+1) l
      | '\n' -> lineNum := !lineNum + 1;handle (n+1) l
      | '\t' -> handle (n+1) l
      | '>' -> handle (n+1) ({label="BTH";content=">";line=(!lineNum)}::l)
      | '<' -> handle (n+1) ({label="LTH";content="<";line=(!lineNum)}::l)
      | '+' -> handle (n+1) ({label="PLUS";content="+";line=(!lineNum)}::l)
      | '-' -> handle (n+1) ({label="MINUS";content="-";line=(!lineNum)}::l)
      | '%' -> handle (n+1) ({label="MOD";content="%";line=(!lineNum)}::l)        
      | '=' -> handle (n+1) ({label="EQU";content="=";line=(!lineNum)}::l)
      | '*' -> handle (n+1) ({label="STAR";content="*";line=(!lineNum)}::l)
      | '(' -> handle (n+1) ({label="LPAR";content="(";line=(!lineNum)}::l)
      | ')' -> handle (n+1) ({label="RPAR";content=")";line=(!lineNum)}::l)
      | ',' -> handle (n+1) ({label="COMMA";content=",";line=(!lineNum)}::l)
      | '.' -> handle (n+1) ({label="DOT";content=".";line=(!lineNum)}::l)
      | ';' -> handle (n+1) ({label="SEMI";content=";";line=(!lineNum)}::l)
      | '\''-> handle (n+1) ({label="SQUT";content="\'";line=(!lineNum)}::l)
      | '/' -> 
      if str.[n+1] = '*' then 
        let nn = ref (n+2) in
          while !nn < len && (String.equal (String.sub str !nn 2) "*/") = false do
            nn := !nn + 1
          done; handle (!nn+2) l
      else handle (n+1) ({label="DIVI";content="*";line=(!lineNum)}::l)
      | '\"' -> 
      let (next, res) = handleString (n+1) [] in
      handle (next+1) ({label="STRING";content=res;line=(!lineNum)}::l)
      | x -> 
      if isNum x then  
            let (next, res) = handleNum n [] in
            handle (next+1) ({label="INT";content=res;line=(!lineNum)}::l)
      else if isLetter x then 
          let (next, res) = handleName n [] in
          if isInStrList res reWords then
            handle (next+1) ((matchReWord res reWordList !lineNum)::l)
          else handle (next+1) ({label="VAR";content=res;line=(!lineNum)}::l)            
      else (error "Illegal word %c\n" x;List.rev l)
      else List.rev l
    in
    List.rev (matchTwo (handle 0 []) [])   
;;

let print_lex {content;label} = Printf.printf "%s\t| %s" label content

let rec print_lex_list l = 
  match l with 
  | [] -> print_newline ()
  | x::xs -> print_lex x;print_newline ();print_lex_list xs
;;  

let a = lexTest (readSans "test.sans")
;;