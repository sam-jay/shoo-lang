open Ast



let fmt_one name v = String.concat "" [name; "("; v; ")"]
let fmt_two name v1 v2 = String.concat "" [name; "("; v1; ","; v2; ")"]
let fmt_three name v1 v2 v3 = String.concat "" [name; "("; v1; ","; v2; ","; v3; ")"]
let fmt_four name v1 v2 v3 v4 = String.concat "" [name; "("; v1; ","; v2; ","; v3; ","; v4; ")"]

let fmt_list l =
  let items = String.concat ";" l in
  String.concat "" ["["; items; "]"]

let fmt_typ = function
    Any -> "Any"
  | Void -> "Void"
  | Func -> "Func"
  | Int -> "Int"
  | Float -> "Float"
  | Bool -> "Bool"
  | String -> "String"

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

and fmt_stmt = function
  Expr(e) -> fmt_expr e
| Return(e) -> fmt_one "Return" (fmt_expr e)
| FDecl(n, p, t, b) -> 
  fmt_four "FDecl" n (fmt_params p) (fmt_typ t) (fmt_stmt_list b)
| VDecl (t, n ) -> fmt_two "VDecl" (fmt_typ t) n
| VDef (t, n, e) -> fmt_three "VDef" (fmt_typ t) n (fmt_expr e)
| ForLoop (e1, e2, e3, s) -> 
  fmt_four "ForLoop" (fmt_expr e1) (fmt_expr e2) 
  (fmt_expr e3) (fmt_stmt_list s)
| If(e, trueL, falseL) -> fmt_three "If" (fmt_expr e) (fmt_stmt_list trueL) (fmt_stmt_list falseL)

and fmt_stmt_list l =
  let stmts = List.map fmt_stmt l in
  String.concat "" ["["; String.concat ", " stmts; "]"]

let fmt_prog program =
  fmt_stmt_list program

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  print_endline (fmt_prog program)
