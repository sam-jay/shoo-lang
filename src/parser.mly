%{ open Ast %}

%token SEMI ASSIGN INT FLOAT STRING BOOL FUNC LPAREN RPAREN LBRACKET RBRACKET
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN NOT DECREMENT INCREMENT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR DOT
%token FOR COMMA RETURN ANY VOID STRUCT COLON IN ARRAY LT GT LSQBRACE RSQBRACE
%token NEW FUNCTION IF ELIF ELSE
%token <int> INTLIT
%token <float> FLOATLIT
%token <bool> BOOLLIT
%token <string> ID
%token <string> STRUCTID
%token <string> STRLIT
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right INCREMENT DECREMENT
%left DOT
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
/*| FUNCTION ID LPAREN params_opt RPAREN ret_typ LBRACKET stmt_list RBRACKET { FDecl($2, $4, $6, List.rev $8) }*/
| RETURN expr SEMI { Return($2) }
| FOR LPAREN opt_loop_init SEMI opt_expr SEMI opt_expr RPAREN LBRACKET stmt_list RBRACKET 
    { ForLoop($3, $5, $7, List.rev $10) }
| FOR LPAREN typ ID IN expr RPAREN LBRACKET stmt_list RBRACKET { EnhancedFor($3, $4, $6, List.rev $9) }
| STRUCT STRUCTID LBRACKET mems_opt RBRACKET { StructDef($2, $4) }
| IF LPAREN expr RPAREN LBRACKET stmt_list RBRACKET false_branch { If($3, $6, $8) }

opt_init:
  { None }
| ASSIGN expr { Some($2) }

false_branch: elif { $1 } | cf_else { $1 } | %prec NOELSE { [] }

elif:
ELIF LPAREN expr RPAREN LBRACKET stmt_list RBRACKET false_branch { [If($3, $6, $8)] }

cf_else:
ELSE LBRACKET stmt_list RBRACKET { $3 }

expr:
  INTLIT { IntLit($1) }
| FLOATLIT { FloatLit($1) }
| BOOLLIT { BoolLit($1) }
| STRLIT { StrLit($1) }
| ID { Id($1) }
| expr INCREMENT { Pop($1, Inc) }
| expr DECREMENT { Pop($1, Dec) }
| expr ASSIGN expr { Assign($1, $3) }
| expr DOT ID { Dot($1, $3) }
| expr PLUS expr { Binop($1, Add, $3) }
| expr MINUS expr { Binop($1, Sub, $3) }
| expr TIMES expr { Binop($1, Mult, $3) }
| expr DIVIDE expr { Binop($1, Div, $3) }
| expr MOD expr { Binop($1, Mod, $3) }
| expr EQ expr { Binop($1, Equal, $3) }
| expr NEQ expr { Binop($1, Neq, $3) }
| expr LT expr { Binop($1, Less, $3) }
| expr LEQ expr { Binop($1, Leq, $3) }
| expr GT expr { Binop($1, Greater, $3) }
| expr GEQ expr { Binop($1, Geq, $3) }
| expr AND expr { Binop($1, And, $3) }
| expr OR expr { Binop($1, Or, $3) }
| ID LSQBRACE expr RSQBRACE { ArrayAccess($1, $3) }
| MINUS expr %prec NEG { Unop(Neg, $2) }
| NOT expr { Unop(Not, $2) }
| NEW LPAREN newable RPAREN { New($3) }
| LBRACKET destruct RBRACKET ASSIGN expr { Destruct(List.rev $2, $5) }
/*| FUNCTION LPAREN params_opt RPAREN ret_typ LBRACKET stmt_list RBRACKET
  { FExpr($3, $5, List.rev $7) }*/
| function_expr { FExpr($1) }
| ID LPAREN args_opt RPAREN { FCall($1, $3) }
| LBRACKET init_list RBRACKET { StructInit(List.rev $2) }
| LSQBRACE opt_items RSQBRACE { ArrayLit($2) }

newable:
  ARRAY LT typ GT LSQBRACE expr RSQBRACE { NArray($3, $6) }
| STRUCTID { NStruct($1) }

function_expr:
    FUNCTION ID LPAREN params_opt RPAREN ret_typ LBRACKET stmt_list 
    RBRACKET
    { { name = $2;
        params = $4;
        typ = $6;
        body = List.rev $8} }

opt_items:
  { [] }
| item_list { List.rev $1 }

item_list:
  expr { [$1] }
| item_list COMMA expr { $3 :: $1 }

opt_loop_init:
  { None }
| expr { Some(Expr($1)) }
| typ ID ASSIGN expr { Some(VDecl($1, $2, Some($4))) }

opt_expr:
  { None }
| expr { Some($1) }

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
| STRUCTID { Struct($1) }

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
