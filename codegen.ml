open Parse
open Symbol

let outname = "test.sans.code"
let outfile = open_out outname

let notNil l = 
  match l with
  | [] -> false
  | _ -> true

let hasChild p = notNil (getPChild p) 
let getFirstChild p = List.hd (getPChild p)
let getSecChild p = List.nth (getPChild p) 1
let needCspS p = hasChild (getSecChild p)
let needCspF p = hasChild (getFirstChild p)
let needCsp2 p = hasChild (getSecChild p) && hasChild (getFirstChild p)


let tyToWd ty =
  match ty with
  | "float" -> "DD"
  | "string" | "char" -> "DB"
  | "int" -> "DW"
  | _ -> "  "

let rec repeat str i = 
  match i with 
  |0 | 1 -> str 
  | x -> str^", "^(repeat str (i-1))

let initToStr ty len node=
  match ty with
  | "float" -> "00000000H"
  | "string" -> "\""^(getPStr node)^"\""
  | "char" -> "00H"
  | "int" -> "0000H"
  | _ -> "  "

let genData name ty len node= name^"\t"^(tyToWd ty)^"\t"^(initToStr ty len node)^"\n"

let opToCmd op = 
  match op with
  | "+" -> "ADD"
  | "-" -> "MINUS"
  | "*" -> "MUTI"
  | "/" -> "DIVI"
  | "%" -> "MOD"
  | x -> x

let genOpr op a b res = "\t"^(op)^"\t"^a^", "^b^", "^res^"\n"

let putLabel label = label^": \n"

let symToData hsl =
  Printf.fprintf outfile "section .data\n";
  Hashtbl.iter (fun x y -> Printf.fprintf outfile "\t%s" (genData x (getTy y) (getLen y) (getPNode y)) ) res
;;
symToData res;; 

let tripTail str = String.sub str 0 (String.length str - 1)

let matchJump l =
  match l with 
  | "BTH"|"LTH" -> "JNS"
  | "BRTH"|"LETH" -> "JS"
  | "EQU" -> "JE"
  | _ -> "JNE"
  
let genLabel len = 
  let gen() = match Random.int(26+26+10) with
        n when n < 26 -> int_of_char 'a' + n
      | n when n < 26 + 26 -> int_of_char 'A' + n - 26
      | n -> int_of_char '0' + n - 26 - 26 in
    let gen _ = String.make 1 (char_of_int(gen())) in
    String.concat "" (Array.to_list (Array.init len gen));;

let rec plToCode pl =
  let rec puToCode pu reg=
    let lb = getPLabel pu in
    match lb with
    |"PAR"| "DECL" -> puToCode (getFirstChild pu) reg
    |"ASSIGN" -> 
      if needCsp2 pu then 
      (puToCode (getFirstChild pu) "rbx")^(puToCode (getSecChild pu) "rbx")^(genOpr "MOV" ("rbx") ("rbx") (getPStr (getFirstChild pu)))
      else if needCspS pu then 
      (puToCode (getSecChild pu) reg)^(genOpr "MOV" reg reg (getPStr (getFirstChild pu)))
      else if needCspF pu then
      (puToCode (getFirstChild pu) reg)^(genOpr "MOV" reg reg (getPStr (getFirstChild pu)))
      else
      genOpr "MOV" (getPStr (getSecChild pu)) (getPStr (getSecChild pu)) (getPStr (getFirstChild pu)) 
    |"PLUS"|"MINUS"|"STAR"|"DIVI" ->
      if needCsp2 pu then 
      (puToCode (getSecChild pu) ("rbx"))^(puToCode (getSecChild pu) ("rdx"))^(genOpr lb ("rbx") ("rdx") reg)
      else if needCspS pu then 
      (puToCode (getSecChild pu) reg)^(genOpr lb reg (getPStr (getFirstChild pu)) reg)
      else if needCspF pu then
      (puToCode (getFirstChild pu) reg)^(genOpr lb reg (getPStr (getSecChild pu)) reg)
      else
      genOpr lb (getPStr (getFirstChild pu)) (getPStr (getSecChild pu)) reg 
    |"PLUSE"|"MINUSE"|"MUTIE"|"DIVIE" ->
      if needCspS pu then 
      (puToCode (getSecChild pu) reg)^(genOpr (tripTail lb) (getPStr (getFirstChild pu)) reg  reg)
      else 
      genOpr (tripTail lb) (getPStr (getFirstChild pu)) (getPStr (getSecChild pu))  reg
    |"BTH"|"LRTH"|"EQU" ->
      if needCspS pu then 
      (puToCode (getSecChild pu) reg)^(genOpr "MINUS" (getPStr (getFirstChild pu)) reg  reg)
      else 
      genOpr "MINUS" (getPStr (getFirstChild pu)) (getPStr (getSecChild pu))  reg
    |"LTH"|"BRTH"  ->
      if needCspS pu then 
      (puToCode (getSecChild pu) reg)^(genOpr "MINUS" reg (getPStr (getFirstChild pu))  reg)
      else 
      genOpr "MINUS" (getPStr (getSecChild pu)) (getPStr (getFirstChild pu)) reg
    | _  -> 
      if hasChild pu then plToCode (getPChild pu)
      else ""
  in
  match pl with
  | [] -> ""
  | x::xs -> 
    match getPLabel x with
    |"WEXPR" ->
      let la = "WH"^(genLabel 3) in
      (puToCode (getFirstChild x) "rbx")^
      (genOpr (matchJump (getPLabel (getFirstChild x))) la la la)^
      (putLabel la)^
      (plToCode (getPChild (getSecChild x)))^
      (puToCode (getFirstChild x) "rbx")^
      (genOpr (matchJump (getPLabel (getFirstChild x))) la la la)^
      (plToCode xs)
    |"IFEXPR" ->
      let la = "IF"^(genLabel 3) in
      let lb = "IF"^(genLabel 3) in
      (puToCode (getFirstChild x) "rbx")^
      (genOpr (matchJump (getPLabel (getFirstChild x))) la la la)^
      (genOpr "JMP" lb lb lb)^
      (putLabel la)^
      (plToCode (getPChild (getSecChild x)))^
      (putLabel lb)^
      (plToCode xs)
    |  _ -> (puToCode x "rbx")^(plToCode xs)
;;

Printf.fprintf outfile "section .text\nglobal mystart\nmystart:\n%s\npush dword 0\nmov eax, 0x1\nsub esp, 4\nINT	0x80\n" (plToCode pl);;
close_out outfile;;
      

 
