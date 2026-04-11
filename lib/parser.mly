  %{
    open Ast
  %}

/* Meta commands */
%token EXIT HELP

/* statements */
%token INSERT SELECT 

/* Operators */
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

/* Datatype literals */
%token <int> INT
%token <float> FLOAT
%token <string> IDENTIFIER

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

%start <Ast.top_level> program

%%

program: 
  | meta ENTER { $1 }
  | statement; ENTER { $1 }
  | meta EOF { $1 }
  | statement; EOF { $1 }
  ;

meta:
  | EXIT              { Meta_command (Exit)}
  | HELP              { Meta_command (Help)}


statement:
  | SELECT; e = expr { Statement (Select e) }
  | INSERT; e = expr { Statement (Insert e) }
  

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

