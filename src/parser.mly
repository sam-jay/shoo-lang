%{ open Ast %}

%token SEMI ASSIGN INT FLOAT STRING BOOL FUNC LPAREN RPAREN LBRACE RBRACE
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token FOR COMMA RETURN ANY VOID STRUCT COLON IN ARRAY LT GT LSQBRACE RSQBRACE
%token NEW FUNCTION IF ELIF ELSE
%token <int> INTLIT
%token <float> FLOATLIT
%token <bool> BOOLLIT
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG

%start program
%type <Ast.program> program

%%

program: stmt_list EOF { List.rev $1 }

stmt_list:
  { [] }
| stmt_list stmt { $2 :: $1 }

stmt:
  expr SEMI { Expr $1 }
| typ ID opt_init SEMI { VDecl($1, $2, $3) }
| FUNCTION ID LPAREN params_opt RPAREN ret_typ LBRACE stmt_list RBRACE { FDecl($2, $4, $6, List.rev $8) }
| RETURN expr SEMI { Return($2) }
| FOR LPAREN opt_expr SEMI expr SEMI opt_expr RPAREN LBRACE stmt_list RBRACE 
    { ForLoop($3, $5, $7, List.rev $10) }
| FOR LPAREN typ ID IN expr RPAREN LBRACE stmt_list RBRACE { EnhancedFor($3, $4, $6, List.rev $9) }
| STRUCT ID LBRACE mems_opt RBRACE { StructDef($2, $4) }
| IF LPAREN expr RPAREN LBRACE stmt_list RBRACE false_branch { If($3, $6, $8) }

opt_init:
  { None }
| ASSIGN expr { Some($2) }

false_branch: elif { $1 } | cf_else { $1 } | %prec NOELSE { [] }

elif:
ELIF LPAREN expr RPAREN LBRACE stmt_list RBRACE false_branch { [If($3, $6, $8)] }

cf_else:
ELSE LBRACE stmt_list RBRACE { $3 }

expr:
| INTLIT { IntLit($1) }
| FLOATLIT { FloatLit($1) }
| BOOLLIT { BoolLit($1) }
| ID { Id($1) }
| ID ASSIGN expr { Assign($1, $3) }
| expr PLUS expr { Binop($1, Add, $3) }
| expr MINUS expr { Binop($1, Sub, $3) } 
| expr TIMES expr { Binop($1, Mult, $3) } 
| expr DIVIDE expr { Binop($1, Div, $3) } 
| expr PLUS expr { Binop($1, Add, $3) }
| expr MINUS expr { Binop($1, Sub, $3) }
| expr TIMES expr { Binop($1, Mult, $3) }
| expr DIVIDE expr { Binop($1, Div, $3) }
| expr EQ expr { Binop($1, Equal, $3) }
| expr NEQ expr { Binop($1, Neq, $3) }
| expr LT expr { Binop($1, Less, $3) }
| expr LEQ expr { Binop($1, Leq, $3) }
| expr GT expr { Binop($1, Greater, $3) }
| expr GEQ expr { Binop($1, Geq, $3) }
| expr AND expr { Binop($1, And, $3) }
| expr OR expr { Binop($1, Or, $3) }
| MINUS expr %prec NEG { Unop(Neg, $2) }
| NOT expr { Unop(Not, $2) }
| NEW LPAREN typ RPAREN { New($3) }
| LBRACE destruct RBRACE ASSIGN expr { Destruct(List.rev $2, $5) }
| FUNCTION LPAREN params_opt RPAREN ret_typ LBRACE stmt_list RBRACE
  { FExpr($3, $5, List.rev $7) }
| ID LPAREN args_opt RPAREN { FCall($1, $3) }
| LBRACE init_list RBRACE { StructInit(List.rev $2) }
| LSQBRACE opt_items RSQBRACE { ArrayLit($2) }


opt_items:
  { [] }
| item_list { List.rev $1 }

item_list:
  expr { [$1] }
| item_list COMMA expr { $3 :: $1 }

opt_expr:
           { NoExpr}
    | expr { $1}

ret_typ:
  VOID { Void }
| typ { $1 }

typ:
  INT { Int }
| FLOAT { Float }
| BOOL { Bool }
| STRING { String }
| ARRAY LT typ GT { Array($3) }
| FUNC { Func }
| ANY { Any }

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

destruct:
  ID SEMI { [$1] }
| destruct ID SEMI { $2 :: $1 }

mems_opt:
  { [] }
| mem_list { List.rev $1 }

mem_list:
  member SEMI { [$1] }
| mem_list member SEMI { $2 :: $1 }

member:
  typ ID { ($1, $2, None) }
| typ ID ASSIGN expr { ($1, $2, Some($4)) }

init_list:
  init SEMI { [$1] }
| init_list init SEMI { $2 :: $1 }

init: ID ASSIGN expr { ($1, $3) }