{
  open Parser
}

let digit = ['0'-'9']
let int = '-'? digit+
let frac = '.'digit*
let float = digit* frac?
let white = [' ' '\t']+
let command = ['a'-'z' 'A'-'Z' '_']+
let meta_command = ['.']['a'-'z' 'A'-'Z' '_']+
let newline = '\r' | '\n' | "\r\n"
let string = [^ '~' '(' ')' '\\' ' ' '\t' '\n']+
(* let id = string (string | digit | '_')* *)

rule read =
  parse
    | "("     { LPAREN }
    | ")"     { RPAREN }
    | "*"     { MULT }
    | "+"     { SUM }
    | "/"     { DIV }
    | "-"     { SUBT }
    | "--"    { comment lexbuf }
    | "TRUE"  { TRUE }
    | "FALSE" { FALSE }
    | ";;"    { ENTER }
    | white   { read lexbuf}
    | int     { INT (int_of_string(Lexing.lexeme lexbuf))}
    | float   { FLOAT (float_of_string(Lexing.lexeme lexbuf))}
    (* Going to want to approach this more explicitly to avoid collisions with ids and vars *)
    | command { COMMAND (Lexing.lexeme lexbuf) }
    | meta_command  { META_COMMAND (Lexing.lexeme lexbuf) }
    | newline { Lexing.new_line lexbuf; read lexbuf }
    | eof     { EOF }
and comment = 
  parse
    | newline { Lexing.new_line lexbuf; read lexbuf}
    | eof { EOF }
    | _ { comment lexbuf }
