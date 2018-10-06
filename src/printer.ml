open Ast



let fmt_one name v = String.concat "" [name; "("; v; ")"]
let fmt_two name v1 v2 = String.concat "" [name; "("; v1; ","; v2; ")"]
let fmt_three name v1 v2 v3 = String.concat "" [name; "("; v1; ","; v2; ","; v3; ")"]
let fmt_four name v1 v2 v3 v4 = String.concat "" [name; "("; v1; ","; v2; ","; v3; ","; v4; ")"]

let fmt_list l =
  let items = String.concat ";" l in
  String.concat "" ["["; items; "]"]

let rec fmt_typ = function
    Any -> "Any"
  | Void -> "Void"
  | Func -> "Func"
  | Int -> "Int"
  | Float -> "Float"
  | Bool -> "Bool"
  | String -> "String"
  | Struct(n) -> fmt_one "Struct" n
  | Array(t, s) -> fmt_two "Array" (fmt_typ t) (match s with 
                                                  Fixed(n) -> fmt_one "Fixed" (string_of_int n) 
                                                | Param(s) -> fmt_one "Param" s)

let fmt_params l =
  let fmt_p = function
    (t, n) -> String.concat "" ["("; fmt_typ t; ", "; n; ")"] in
  fmt_list (List.map fmt_p l)

let rec fmt_expr = function
  IntLit(l) -> fmt_one "IntLit" (string_of_int l)
| FloatLit(l) -> fmt_one "FloatLit" (string_of_float l)
| StrLit(l) -> fmt_one "StrLit"  l
| BoolLit(l) -> fmt_one "BoolLit" (string_of_bool l)
| Id(s) -> fmt_one "Id" s
| Assign(s, e) -> fmt_two "Assign" s (fmt_expr e)
| FCall(n, a) -> fmt_two "FCall" n (fmt_list (List.map fmt_expr a))
| FExpr(p, t, b) -> fmt_three "FExpr" (fmt_params p) (fmt_typ t) (fmt_stmt_list b)
| NoExpr -> "NoExpr"
| StructInit(l) -> fmt_one "StructInit" (fmt_init l)
| ArrayLit(l) -> fmt_one "ArrayLit" (fmt_list (List.map fmt_expr l))
| Destruct(l, e) -> fmt_two "Destruct" (fmt_list l) (fmt_expr e)
| New(t) -> fmt_one "New" (fmt_typ t)

and fmt_members l =
  let fmt_m = function
    (t, n, None) -> fmt_three "" (fmt_typ t) n "None"
  | (t, n, Some(e)) -> fmt_three "" (fmt_typ t) n (fmt_expr e) in
  fmt_list (List.map fmt_m l)

and fmt_init l =
 let fmt_i (n, e) = fmt_two "" n (fmt_expr e) in
 fmt_list (List.map fmt_i l)

and fmt_stmt = function
  Expr(e) -> fmt_expr e
| Return(e) -> fmt_one "Return" (fmt_expr e)
| FDecl(n, p, t, b) -> 
  fmt_four "FDecl" n (fmt_params p) (fmt_typ t) (fmt_stmt_list b)
| VDecl (t, n, l) -> fmt_three "VDecl" (fmt_typ t) n (match l with None -> "" | Some(e) -> fmt_expr e)
| ForLoop (e1, e2, e3, s) -> 
  fmt_four "ForLoop" (fmt_expr e1) (fmt_expr e2) 
  (fmt_expr e3) (fmt_stmt_list s)
| StructDef(n, m) -> fmt_two "StructDef" n (fmt_members m)
| EnhancedFor(t, n, e, b) -> fmt_four "EnhancedFor" (fmt_typ t) n (fmt_expr e) (fmt_stmt_list b)
| If(e, tL, fL) -> fmt_three "If" (fmt_expr e) (fmt_stmt_list tL) (fmt_stmt_list fL)

and fmt_stmt_list l =
  let stmts = List.map fmt_stmt l in
  fmt_list stmts

let fmt_prog program =
  fmt_stmt_list program

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  print_endline (fmt_prog program)
