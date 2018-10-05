%{ open Ast %}

%token EOF SEMI ASSIGN INT FLOAT STRING BOOL FUNC LPAREN RPAREN LBRACE RBRACE
%token FOR COMMA RETURN ANY VOID
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
| typ ID ASSIGN expr SEMI { VDef($1, $2, $4) }
| typ ID SEMI { VDecl($1, $2) }
| FUNC ID LPAREN params_opt RPAREN ret_typ LBRACE stmt_list RBRACE { FDecl($2, $4, $6, $8) }
| RETURN expr SEMI { Return($2) }
| FOR LPAREN opt_expr SEMI expr SEMI opt_expr RPAREN LBRACE stmt_list RBRACE 
    { ForLoop($3, $5, $7, $10) }

expr:
| INTLIT { IntLit($1) }
| FLOATLIT { FloatLit($1) }
| BOOLLIT { BoolLit($1) }
| ID { Id($1) }
| ID ASSIGN expr { Assign($1, $3) }
| FUNC LPAREN params_opt RPAREN ret_typ LBRACE stmt_list RBRACE 
  { FExpr($3, $5, $7) }
| ID LPAREN args_opt RPAREN { FCall($1, $3) }

opt_expr:
           { NoExpr}
    | expr { $1}

ret_typ:
  ANY { Any }
| VOID { Void }
| typ { $1 }

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
