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
  | i = INT                   { Int i }
  | f = FLOAT                 { Float f }
  | c = COMMAND               { Command c }
  | mc = META_COMMAND         { Meta_Command mc }
  | e1 = expr; SUM; e2 = expr {Binop (Add, e1, e2)}
  | e1 = expr; SUBT; e2 = expr {Binop (Subt, e1, e2)}
  | e1 = expr; MULT; e2 = expr  {Binop (Mult, e1, e2)}
  | e1 = expr; DIV; e2 = expr  {Binop (Div, e1, e2)}
  | LPAREN; e = expr; RPAREN { e }
  ;

