open Ast

type snewable =
  SNArray of typ * sexpr
| SNStruct of string

and sexpr = typ * sx
and sx =
  SIntLit of int
| SFloatLit of string
| SStrLit of string
| SBoolLit of bool
| SArrayLit of sexpr list
| SId of string
| SBinop of sexpr * op * sexpr
| SUnop of uop * sexpr
| SPop of sexpr * pop 
| SAssign of sexpr * sexpr
| SFCall of string * sexpr list
| SFExpr of sfexpr
| SStructInit of (string * sexpr) list
| SDestruct of string list * sexpr
| SArrayAccess of string * sexpr
| SDot of sexpr * string
| SNew of snewable
| SClosure of sclsr
| SNoexpr

and sfexpr = {
    sname : string;
    srecursive : bool; 
    styp : typ;
    sparams: bind list;
    sbody : sstmt list
}

and sclsr = {
  ind: int;
  free_vars: bind list;
}

and sstmt =
  SExpr of sexpr
| SVDecl of typ * string * sexpr option
| SReturn of sexpr
| SIf of sexpr * sstmt list * sstmt list
| SForLoop of (sstmt option) * (sexpr option) * (sexpr option) * sstmt list
| SStructDef of string * (typ * string * sexpr option) list
| SEnhancedFor of typ * string * sexpr * sstmt list

type sprogram = sstmt list
