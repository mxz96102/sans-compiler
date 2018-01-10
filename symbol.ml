open Parse

type sbl = {
  name : string;
  ty : string;
  scope : int;
  declare : int;
  len : int;
  address : int;
  use : int list;
  pNode : parseUnit;
}

type sblHash = {
  parent : sblHash;
  hs : (string, sbl) Hashtbl.t
}

let rec isInStrList str sl=
  match sl with 
  | [] -> false
  | [x] -> if String.equal str x then true else false    
  | x::xs -> if String.equal str x then true else isInStrList str xs

let getName {name} = name
let getTy {ty} = ty
let getScope {scope} = scope
let getParent {parent} = parent
let getHs {hs} = hs
let getDeclare {declare} = declare
let getUse {use} = use
let getLen {len} = len
let getAddr {address} = address
let getPNode {pNode} = pNode

let addUse {name;ty;scope;declare;use;address;len;pNode} l= {name;ty;scope;declare;use=l::use;address;len;pNode}
let setAddr {name;ty;scope;declare;use;address;len;pNode} l= {name;ty;scope;declare;use;address=l;len;pNode}

let isInHash sbl hs = Hashtbl.mem hs (getName sbl)

let addToHash sbl hs = Hashtbl.add hs (getName sbl) sbl

let rec addString l=
  match getPLabel l with
  | "STRING" -> (getPStr l)
  | "PLUS" -> (
    match getPChild l with 
    | [] -> ""
    | x1::x2::xs -> (addString x1)^(addString x2)
    | _ -> ""
  )
  | _ -> (print_string "error string operation at line ";print_int (getPLine l);print_string "\n";"")

let getTLenth n l = 
  match n with
  | "int" -> 2
  | "float" -> 4
  | "char" -> 1
  | "string" -> String.length (addString l) * 1
  | _ -> 0

let searchSymbol pl= 
  let scp = 1 in
  let addr = ref 0 in 
  let hsl = Hashtbl.create 32 in
  let rec search pll =
  match pll with
  | [] -> ()
  | x::xs -> 
  match getPLabel x with
  | "DECL" -> 
    let ty = getPStr x in
    let len = (getTLenth ty (List.nth (getPChild (List.hd (getPChild x))) 1)) in
    let varName = getPStr (List.hd (getPChild (List.hd (getPChild x)))) in
    let oneSbl = {name = varName; ty = ty; scope = scp;declare=(getPLine x);use=[];address=0;len;pNode=x} in
    if isInHash oneSbl hsl then 
    print_string ("! The var "^varName^" is already declared\n")
    else 
    (addToHash (setAddr oneSbl !addr) hsl;
    addr:= !addr + len;
    search (List.tl (getPChild (List.hd (getPChild x)))));
    search xs
  | "VAR" ->
    let varName = getPStr x in
    let oneSbl = {name = varName; ty = ""; scope = 0;declare=(getPLine x);use=[];address=0;len=0;pNode=x} in
    if (isInHash oneSbl hsl) = false then 
    print_string ("! The var "^varName^"is not declared\n")
    else 
    let a = Hashtbl.find hsl varName in
    Hashtbl.replace hsl varName (addUse a (getPLine x))
  | _ ->
    let rest = getPChild x in 
    search rest;search xs
  in
  search pl; hsl
;;

let calType l r line=
  if String.equal l r then l 
  else match (l,r) with
  | _ -> Printf.printf "error type %s %s at line %d\n" l r line; "error"

let rec getType l hsl=
  match getPLabel l with
  | "VAR" -> getTy (Hashtbl.find hsl (getPStr l))
  | "INT" -> "int"
  | "CHAR" -> "char"
  | "FLOAT" -> "float"
  | "STRING" -> "string"
  | "DECL" -> getPStr l
  | _ -> 
  match getPChild l with
  | [] -> Printf.printf "null type %s at line %d\n" (getPLabel l) (getPLine l) ; "null"
  | x1::x2::xs -> calType (getType x1 hsl) (getType x2 hsl) (getPLine l)
  | x::xs -> getType x hsl

let rec parseType pt hsl=
  match getPLabel pt with
  | "DECL" -> 
    if String.equal (getPLabel (List.hd (getPChild pt))) "ASSIGN" then
    calType (getType pt hsl) (getType (List.hd (getPChild pt)) hsl) (getPLine pt)
    else ((print_string ("declare "^(getPStr pt)^" error at line %d\n")) ;"not pass")
  | x ->
  if isInStrList x twoOprWord then getType pt hsl
  else 
  match (getPChild pt) with
  | [] -> ""
  | _ ->
  List.hd (List.map (fun xx -> (parseType xx hsl)) (getPChild pt))

let rec print_use l =
  match l with
  | [] -> print_string "\n"
  | x::xs -> Printf.printf "%d," x; print_use xs ;; 

let pl = (parseLex a);;  
let res = (searchSymbol pl);;  

parseType (List.hd pl) res;;

print_newline ();
print_string "name\ttype\tlength\taddress\tdeclare\tscope\tuse\n";  
Hashtbl.iter (fun x y -> Printf.printf "%s\t%s\t%d\t%06XH\t%d\t%d\t" x (getTy y) (getLen y) (getAddr y) (getDeclare y) (getScope y);print_use (getUse y)) res