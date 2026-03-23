%token <int> INT
%token <string> META_COMMAND
%token <string> COMMAND
%token EOF

%start <Ast.expr> prog

%%

prog: 
  | e = expr; EOF { e }
  ;

expr:
  | i = INT           { Int i }
  | c = COMMAND       { COMMAND c }
  | mc = META_COMMAND { META_COMMAND mc }
