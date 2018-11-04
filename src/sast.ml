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

let string_of_sstmt = function
| SExpr(_) -> "SExpr"
| _ -> "Other"

(*PRETTY PRINTING based off of printer.ml*)

let fmt_rec = function
  true -> "recursive"
| false -> "not_recursive"

let fmt_sparams l =
  let fmt_p = function
    (t, n) -> String.concat "" ["("; fmt_typ t; ", "; n; ")"] in
  fmt_list (List.map fmt_p l)

let rec fmt_sexpr (t,s) =
  "(" ^ fmt_typ t ^ " : " ^ (match s with
  SIntLit(l) -> fmt_one "IntLit" (string_of_int l)
| SFloatLit(l) -> fmt_one "FloatLit" l
| SStrLit(l) -> fmt_one "StrLit"  l
| SBoolLit(l) -> fmt_one "BoolLit" (string_of_bool l)
| SId(s) -> fmt_one "Id" s
| SBinop(e1, o, e2) -> fmt_three "Binop" (fmt_sexpr e1) (fmt_op o) (fmt_sexpr e2)
| SUnop(uo, e) -> fmt_two "Unop" (fmt_uop uo) (fmt_sexpr e)
| SPop(e, po) -> fmt_two "Pop" (fmt_sexpr e) (fmt_pop po)
| SAssign(e1, e2) -> fmt_two "Assign" (fmt_sexpr e1) (fmt_sexpr e2)
| SArrayAccess(s, e) -> fmt_two "ArrayAccess" s (fmt_sexpr e)
| SDot(e, s) -> fmt_two "Dot" (fmt_sexpr e) s
| SFCall(n, a) -> fmt_two "FCall" n (fmt_list (List.map fmt_sexpr a))
(* below actually is parsed with {name = e.name; param = e.params;
 * typ = e.typ; body = e.body}. See test programs for examples. *)
| SFExpr(s) -> fmt_four "FExpr" (fmt_rec s.srecursive) (fmt_sparams s.sparams) 
        (fmt_typ s.styp) (fmt_sstmt_list s.sbody)
| SStructInit(l) -> fmt_one "StructInit" (fmt_sinit l)
| SArrayLit(l) -> fmt_one "ArrayLit" (fmt_list (List.map fmt_sexpr l))
| SDestruct(l, e) -> fmt_two "Destruct" (fmt_list l) (fmt_sexpr e)
| SNew(t) -> fmt_one "New" (fmt_sn t)
| SNoexpr -> ""
          ) ^ ")" 

and fmt_sn = function
  SNArray(t, s) -> fmt_two "NArray" (fmt_typ t) (fmt_sexpr s)
| SNStruct(n) -> fmt_one "NStruct" n

and fmt_smembers l =
  let fmt_m = function
    (t, n, None) -> fmt_three "" (fmt_typ t) n "None"
  | (t, n, Some(e)) -> fmt_three "" (fmt_typ t) n (fmt_sexpr e) in
  fmt_list (List.map fmt_m l)

and fmt_sinit l =
 let fmt_i (n, e) = fmt_two "" n (fmt_sexpr e) in
 fmt_list (List.map fmt_i l)

and fmt_sstmt = function
  SExpr(se) -> fmt_sexpr se
| SReturn(e) -> fmt_one "Return" (fmt_sexpr e)
| SFDecl(n, p, t, b) -> 
  fmt_four "FDecl" n (fmt_sparams p) (fmt_typ t) (fmt_sstmt_list b)
| SVDecl (t, n, l) -> fmt_three "VDecl" (fmt_typ t) n (match l with None -> "" | Some(e) -> fmt_sexpr e)
| SForLoop (init, e2, e3, s) -> 
  fmt_four "ForLoop" 
  (match init with None -> "" | Some(s) -> fmt_sstmt s)
  (fmt_opt_sexpr e2) 
  (fmt_opt_sexpr e3) (fmt_sstmt_list s)
| SStructDef(n, m) -> fmt_two "StructDef" n (fmt_smembers m)
| SEnhancedFor(t, n, e, b) -> fmt_four "EnhancedFor" (fmt_typ t) n (fmt_sexpr e) (fmt_sstmt_list b)
| SIf(e, tL, fL) -> fmt_three "If" (fmt_sexpr e) (fmt_sstmt_list tL) (fmt_sstmt_list fL)

and fmt_sstmt_list l =
  let sstmts = List.map fmt_sstmt l in
  fmt_list sstmts

and fmt_opt_sexpr = function
  None -> ""
| Some(e) -> fmt_sexpr e

let string_of_sprogram sast =
  fmt_sstmt_list sast
