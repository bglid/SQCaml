{
  open Parser
  exception Lexing_error of string
  let meta_cmds = Hashtbl.create 20
  let () = 
    List.iter (fun (strcmd, mcmd) -> Hashtbl.add meta_cmds strcmd mcmd)
      [
      ".exit", EXIT;
      ".help", HELP;
      ".tree", PRINT_TREE;
    ]

  let stmts = Hashtbl.create 20
  let () = 
    List.iter (fun (word, stmt) -> Hashtbl.add stmts word stmt)
      [
      "INSERT", INSERT;
      "SELECT", SELECT;
      "INTO", INTO;
      "VALUES", VALUES;
      "FROM", FROM;
      "WHERE", WHERE;
    ]
}

let digit = ['0'-'9']
let int = '-'? digit+
let frac = '.'digit*
let float = digit* frac?
let white = [' ' '\t']+
let meta_command = ['.']['a'-'z' 'A'-'Z' '_']+
let statement = ['A'-'Z' '_']['A'-'Z' '_']+
let id = ['A'-'Z' 'a'-'z'  '_' ]['a'-'z'  '_' '0'-'9']*
let newline = '\r' | '\n' | "\r\n"




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
    | "=="    { STRUCT_COMP }
    | "<>"    { NEQ }
    | "<="    { LEQ }
    | ">="    { GEQ }
    | "<"     { LT }
    | ">"     { GT }
    | ","     { COMMA }
    | white   { read lexbuf}
    | int     { INT (int_of_string(Lexing.lexeme lexbuf))}
    | float   { FLOAT (float_of_string(Lexing.lexeme lexbuf))}
    | meta_command as md
        {try Hashtbl.find meta_cmds md 
          with Not_found -> UNK_COM md}
    | statement as s
        {try Hashtbl.find stmts s
    with Not_found -> UNK_COM s}
    | id as s { IDENTIFIER s }
    | newline { Lexing.new_line lexbuf; read lexbuf }
    | "'"     { read_string (Buffer.create 16) lexbuf}
    | eof     { EOF }
    | _       { raise (Lexing_error ("bad char, error"))}
and comment = 
  parse
    | newline { Lexing.new_line lexbuf; read lexbuf}
    | eof { EOF }
    | _ { comment lexbuf }
and read_string buff = parse
  | "'"       { STRING (Buffer.contents buff)}
  | _ as c    { Buffer.add_char buff c; read_string buff lexbuf }
  | eof       { raise (Lexing_error ("String not finished, error"))}
