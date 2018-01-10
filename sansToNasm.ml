let read_in_line filename =
    let lines = ref [] in
    let chan = open_in filename in
    try
      while true; do
        lines := input_line chan :: !lines
      done; !lines
    with End_of_file ->
      close_in chan;
      List.rev !lines ;;

let file_lines  = read_in_line "test.sans.code" ;;

let rec reModify line n tmp l=
    if n = (String.length line) - 1 then
      List.rev ((tmp^(String.make 1 (line.[n])))::l)
    else match line.[n] with
    |',' |'\t' -> 
      if String.equal tmp "" then
        reModify line (n+1) tmp l
       else reModify line (n+1) "" (tmp::l)
    | ' ' -> reModify line (n+1) tmp l
    | x -> reModify line (n+1) (tmp^(String.make 1 x)) l
;;  


let handleLine line = reModify line 0 "" []
;;

let rec reArrange x= 
  match List.nth x 0 with
   | "PLUS" ->
    ("\tMOV\t"^"rax, "^(List.nth x 1)^"\n")::
    ("\tMOV\t"^"rdx, "^(List.nth x 2)^"\n")::
    ("\tADD\t"^"rdx, rbx\n")::
    ("\tMOV\t"^(List.nth x 3)^", rdx\n")::[]
   | "STAR" -> 
    ("\tMOV\t"^"rax, "^(List.nth x 1)^"\n")::
    ("\tMOV\t"^"rdx, "^(List.nth x 2)^"\n")::
    ("\tMUL\t"^"rdx, rbx\n")::[]
   | "MINUS" ->
   ("\tMOV\t"^"rax, "^(List.nth x 1)^"\n")::
    ("\tMOV\t"^"rdx, "^(List.nth x 2)^"\n")::
    ("\tMUL\t"^"rdx, rbx\n")::
    ("\tSUB\t"^(List.nth x 3)^", rdx\n")::[]
   | "DIVI" ->
    ("\tMOV\t"^"rax, "^(List.nth x 1)^"\n")::
    ("\tMOV\t"^"rdx, "^(List.nth x 2)^"\n")::
    ("\tDIV\t"^"rdx, rbx\n")::
    ("\tMOV\t"^(List.nth x 3)^", rdx\n")::[]
   | "MOV" ->
    ("\tMOV\t"^"rax"^", "^(List.nth x 1)^"\n")::
    ("\tMOV\t"^(List.nth x 3)^", "^"rax"^"\n")
    ::[]
   | some -> 
    ("\t"^some^"\t"^(List.nth x 1)^"\n")::[]
;; 

let rec rechange lines=
  match lines with
  | [] -> []
  | x::xs -> 
    match x.[0] with
    | '\t' -> List.concat [(reArrange (handleLine x));(rechange xs)]
    | _ -> List.concat [[x];["\n"];(rechange xs)]
;;

let output_line filename l=
  let chan = open_out filename in
  let rec out_lines ll =
    match ll with
    | [] -> Printf.fprintf chan "\n"  
    | x::xs ->  (Printf.fprintf chan "%s" x);out_lines xs
  in
  out_lines (l);
  close_out chan
;;


output_line "test.sans.asm" (rechange file_lines)
;;
  