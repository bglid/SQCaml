{
  open Parser
}

let digit = ['0'-'9']
let int = '-'? digit+
let white = [' ' '\t']+
let command = '-'? '-'? ['a'-'z' 'A'-'Z' '_']+
let meta_command = ['.']['a'-'z' 'A'-'Z' '_']+

rule read =
  parse
  | "*"     { MULT }
  | "+"     { SUM }
  | white   { read lexbuf}
  | int     { INT (int_of_string(Lexing.lexeme lexbuf))}
  | command { COMMAND (Lexing.lexeme lexbuf) }
  | meta_command  { META_COMMAND (Lexing.lexeme lexbuf) }
  | eof     { EOF }
