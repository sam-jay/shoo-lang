%{ open Ast %}

%token EOF SEMI ASSIGN INT FLOAT STRING BOOL FUNC LPAREN RPAREN LBRACE RBRACE COMMA SHOO
%token <int> INTLIT
%token <float> FLOATLIT
%token <bool> BOOLLIT
%token <string> ID

%start program
%type <Ast.program> program

%%

program: stmt_list EOF { List.rev $1 }

stmt_list:
  { [] }
| stmt_list stmt { $2 :: $1 }

stmt:
  expr SEMI { Expr $1 }

expr:
| INTLIT { IntLit($1) }
| FLOATLIT { FloatLit($1) }
| BOOLLIT { BoolLit($1) }
| ID { Id($1) }
| ID ASSIGN expr { Assign($1, $3) }
| named_func { $1 }
| anon_func { $1 }
| SHOO expr { Shoo($2) }
| ID LPAREN args_opt RPAREN { FCall($1, $3) }

named_func: FUNC ID LPAREN params_opt RPAREN LBRACE stmt_list RBRACE { FDecl($2, $4) }

anon_func: FUNC LPAREN params_opt RPAREN LBRACE stmt_list RBRACE { FDecl("", $3) }

typ:
  INT { Int }
| FLOAT { Float }
| BOOL { Bool }
| STRING { String }

params_opt:
  { [] }
| param_list { List.rev $1 }

param_list:
  typ ID { [($1, $2)] }
| param_list COMMA typ ID { ($3, $4) :: $1 }

args_opt:
  { [] }
| arg_list { List.rev $1 }

arg_list:
  expr { [$1] }
| arg_list COMMA expr { $3 :: $1 }