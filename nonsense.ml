let isNum c = c >= '0' && c <= '9'
let isDigit c = c >= 'a' && c<= 'Z'
let kmp target source = 
  let tlen = String.length target in
  let slen = String.length source in
  let rec compare start  = 
    if tlen < (slen - start) then
      if String.equal target (String.sub source start tlen) then start
      else compare (start + 1)
    else
      -1
    in
  compare 0
;;

print_int (kmp "waw" "wwwawwaa");print_newline ();;

class stateNode content last isEnd next = 
    object(self)
      val content = content;
      val last = last;
      val isEnd = isEnd;
      val mutable next = next
  end;;


let list_of_string str =
  let res = ref [] in
  for i = 0 to (String.length str) - 1 do
    res := str.[i] :: !res
  done; 
  List.rev !res
;;  

let isNil l =
  match l with
  | [] -> true
  | _ -> false
 ;; 
let rec handle_list l = 
  match l with
  | [] -> []
  | x1::x2::xs -> 
    new stateNode x2 x1 (isNil (x2::xs)) handle_list :: []
  | x::xs -> []
;;  

