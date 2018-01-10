type lexUnit = { label: string; content: string;line:int}
val readSans : string -> string
val print_lex_list : lexUnit list -> unit
val lexTest : string -> lexUnit list
val reWordList : (string * lexUnit) list
val isInStrList : string -> string list -> bool