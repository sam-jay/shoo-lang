open Ast



let fmt_one name v = String.concat "" [name; "("; v; ")"]
let fmt_two name v1 v2 = String.concat "" [name; "("; v1; ","; v2; ")"]

let fmt_list l =
  let items = String.concat ";" l in
  String.concat "" ["["; items; "]"]

let fmt_params l =
  let fmt_t = function
    Int -> "Int"
  | Float -> "Float"
  | Bool -> "Bool"
  | String -> "String" in
  let fmt_p = function
    (t, n) -> String.concat "" ["("; fmt_t t; ", "; n; ")"] in
  fmt_list (List.map fmt_p l)

let rec fmt_expr = function
  IntLit(l) -> fmt_one "IntLit" (string_of_int l)
| FloatLit(l) -> fmt_one "FloatLit" (string_of_float l)
| StrLit(l) -> fmt_one "StrLit"  l
| BoolLit(l) -> fmt_one "BoolLit" (string_of_bool l)
| Id(s) -> fmt_one "Id" s
| Assign(s, e) -> fmt_two "Assign" s (fmt_expr e)
| FDecl(n, p) -> fmt_two "FDecl" n (fmt_params p)
| FCall(n, a) -> fmt_two "FCall" n (fmt_list (List.map fmt_expr a))

let fmt_stmt = function
  Block(_) -> "Block"
| Expr(e) -> fmt_expr e

let fmt_prog program =
  let stmts = List.map fmt_stmt program in
  String.concat "\n" stmts

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  print_endline (fmt_prog program)