%token EOF

%start <unit> 

%%

prog: 
        | EOF { () }
        ;
