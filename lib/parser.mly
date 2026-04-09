  %{
    open Ast
  %}


%token <int> INT
%token <float> FLOAT
%token <string> META_COMMAND
%token <string> COMMAND
%token LPAREN "("
%token RPAREN ")"
%token SUM
%token MULT
%token DIV
%token SUBT
%token EOF
%token ENTER
%token TRUE
%token FALSE
%token STRUCT_COMP
%token NEQ 
%token LT
%token GT
%token LEQ
%token GEQ

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

%start <Ast.expr> prog

%%

prog: 
  | e = expr; ENTER { e }
  | e = expr; EOF { e }
  ;
  

expr:
  | i = INT           { Int i }
  | f = FLOAT         { Float f }
  | c = COMMAND       { Command c }
  | mc = META_COMMAND { Meta_Command mc }
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
  ;

