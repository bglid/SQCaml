  %{
    open Ast
  %}

/* Meta commands */
%token EXIT HELP PRINT_TREE

/* statements */
%token INSERT SELECT INTO VALUES FROM
%token <string> UNK_COM 

/* Operators */
%token LPAREN "("
%token RPAREN ")"
%token COMMA
%token SUM
%token MULT
%token DIV
%token SUBT
%token EOF
%token TRUE
%token FALSE
%token STRUCT_COMP
%token NEQ 
%token LT
%token GT
%token LEQ
%token GEQ

/* Datatype literals */
%token <int> INT
%token <float> FLOAT
%token <string> IDENTIFIER
%token <string> STRING

%left LT
%left GT
%left LEQ
%left GEQ
%left NEQ
%left STRUCT_COMP
%left SUM
%left SUBT
%left MULT
%left DIV

%start <Ast.top_level> prog

%%

prog: 
  | meta EOF { $1 }
  | statement; EOF { $1 }
  | e = expr; EOF { Statement (Expr e ) }
  ;

meta:
  | EXIT              { Meta_command (Exit)}
  | HELP              { Meta_command (Help)}
  | PRINT_TREE        { Meta_command (Tree)}

field:
  | IDENTIFIER { $1 }
  ;

constant:
  | STRING            { Constant.make_string $1 }
  | INT               { Constant.make_int (Int32.of_int $1) }


constant_list:
  | constant { [$1] }
  | constant COMMA constant_list {$1 :: $3}


statement:
  (* | SELECT; e = expr { Statement (Select e) } *)
  | SELECT LPAREN field_list RPAREN FROM IDENTIFIER {Statement (Select (Select.make $3))}
  | INSERT INTO IDENTIFIER LPAREN field_list RPAREN VALUES LPAREN constant_list RPAREN { Statement ( Insert (Insert.make $5 $9))}
  | unk = UNK_COM; {Statement (Unk_stmt ("error: " ^ unk ^ " is an unknown command") )}


field_list:
  | field { [$1] }
  | field COMMA field_list { $1 :: $3 }
  ;


expr:
  | i = INT           { Int i }
  | f = FLOAT         { Float f }
  | TRUE              { Bool true }
  | FALSE             { Bool false }
  | e1 = expr; SUM; e2 = expr 
                      {Binop (Add, e1, e2)}
  | e1 = expr; SUBT; e2 = expr 
                      {Binop (Subt, e1, e2)}
  | e1 = expr; MULT; e2 = expr  
                      {Binop (Mult, e1, e2)}
  | e1 = expr; DIV; e2 = expr  
                      {Binop (Div, e1, e2)}
  | LPAREN; e = expr; RPAREN { e }
  | e1 = expr; LT; e2 = expr { Binop (Lt, e1, e2) } 
  | e1 = expr; GT; e2 = expr { Binop (Gt, e1, e2) } 
  | e1 = expr; LEQ; e2 = expr { Binop (Leq, e1, e2) } 
  | e1 = expr; GEQ; e2 = expr { Binop (Geq, e1, e2) } 
  | e1 = expr; NEQ; e2 = expr { Binop (Neq, e1, e2) } 
  | e1 = expr; STRUCT_COMP; e2 = expr { Binop (Comp, e1, e2) } 
  ;

