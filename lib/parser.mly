%token <int> INT
%token <string> META_COMMAND
%token <string> COMMAND
%token SUM
%token MULT
%token EOF

%left SUM
%left MULT

%start <Ast.expr> prog

%%

prog: 
  | e = expr; EOF { e }
  ;

expr:
  | i = INT           { Int i }
  | c = COMMAND       { Command c }
  | mc = META_COMMAND { Meta_Command mc }
  | e1 = expr; SUM; e2 = expr {Binop (Add, e1, e2)}
  | e1 = expr; MULT; e2 = expr {Binop (Mult, e1, e2)}
  ;
