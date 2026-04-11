{
  open Parser
  let meta_cmds = Hashtbl.create 20
  let () = 
    List.iter (fun (strcmd, mcmd) -> Hashtbl.add meta_cmds strcmd mcmd)
      [
      ".exit", EXIT;
      ".help", HELP;
    ]

  let stmts = Hashtbl.create 20
  let () = 
    List.iter (fun (word, stmt) -> Hashtbl.add stmts word stmt)
      [
      "insert", INSERT;
      "select", SELECT;
    ]
}

let digit = ['0'-'9']
let int = '-'? digit+
let frac = '.'digit*
let float = digit* frac?
let white = [' ' '\t']+
let meta_command = ['.']['a'-'z' 'A'-'Z' '_']+
let statement = ['a'-'z' 'A'-'Z' '_']+
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
    | "<"     { LT }
    | ">"     { GT }
    | "<="    { LEQ }
    | ">="    { GEQ }
    | ";;"    { ENTER }
    | white   { read lexbuf}
    | int     { INT (int_of_string(Lexing.lexeme lexbuf))}
    | float   { FLOAT (float_of_string(Lexing.lexeme lexbuf))}
    | meta_command as md
        {try Hashtbl.find meta_cmds md 
          with Not_found -> IDENTIFIER md}
    | statement as s
        {try Hashtbl.find stmts (String.uppercase_ascii s)  
          with Not_found -> IDENTIFIER s}
    | newline { Lexing.new_line lexbuf; read lexbuf }
    | eof     { EOF }
    (* | _ { raise (Failure ("Char not allowed in text: " ^Lexing.lexeme lexbuf^)} *)
and comment = 
  parse
    | newline { Lexing.new_line lexbuf; read lexbuf}
    | eof { EOF }
    | _ { comment lexbuf }
