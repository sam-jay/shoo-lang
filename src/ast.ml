module StringMap = Map.Make (String)

type size =
  Fixed of int
| Param of string

type op = 
  Add 
| Sub 
| Mult 
| Div 
| Equal
| Mod
| Neq 
| Less 
| Leq 
| Greater
| Geq 
| And 
| Or

type uop = 
  Neg 
| Not

type pop = 
| Dec 
| Inc

type typ =
  Void
| Int
| Float
| Bool
| String
| Func of func_typ
| Struct of struct_typ
| Array of typ
| ABSTRACT

and func_typ = {
  param_typs: typ list;
  return_typ: typ;
}

and struct_typ = {
  struct_name: string;
  members: (typ * expr option) StringMap.t;
  incomplete: bool;
}

and bind = typ * string

and newable =
  NArray of typ * expr
| NStruct of string

and expr =
  IntLit of int
| FloatLit of string 
| StrLit of string
| BoolLit of bool
| ArrayLit of expr list
| Id of string
| Binop of expr * op * expr
| Unop of uop * expr
| Pop of expr * pop 
| Assign of expr * expr
| FCall of expr * expr list
| FExpr of fexpr
| StructInit of (string * expr) list
| ArrayAccess of expr * expr
| ArrayLength of expr
| Dot of expr * string
| New of newable
| Noexpr

and fexpr = {
  name : string;
  typ : typ;
  params: bind list;
  body : stmt list
}

and stmt =
  Expr of expr
| Destruct of stmt list * expr * string
| VDecl of typ * string * expr option
| Return of expr
| If of expr * stmt list * stmt list
| ForLoop of (stmt option) * (expr option) * (expr option) * stmt list
| WhileLoop of (expr option) * stmt list
| StructDef of string * (typ * string * expr option) list
| EnhancedFor of typ * string * expr * stmt list

type program = stmt list

let fmt_one name v = String.concat "" [name; "("; v; ")"]
let fmt_two name v1 v2 = String.concat "" [name; "("; v1; ","; v2; ")"]
let fmt_three name v1 v2 v3 = String.concat "" 
  [name; "("; v1; ","; v2; ","; v3; ")"]
let fmt_four name v1 v2 v3 v4 = String.concat "" 
  [name; "("; v1; ","; v2; ","; v3; ","; v4; ")"]
let fmt_five name v1 v2 v3 v4 v5 = String.concat "" 
  [name; "("; v1; ","; v2; ","; v3; ","; v4; ","; string_of_bool v5; ")"]

let fmt_list l =
  let items = String.concat ";" l in 
  String.concat "" ["["; items; "]"]

let rec fmt_typ = function
  Void -> "void"
  | Func(e) -> "func(" ^ (String.concat "," (List.map fmt_typ e.param_typs)) 
    ^ "; " ^ (fmt_typ e.return_typ) ^ ")" 
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | String -> "string"
  | Struct(st) -> fmt_three "struct" st.struct_name 
        (fmt_list (List.map (fun (k, (* TODO delete this? (*v*)*)_) -> k) 
            (StringMap.bindings st.members))) 
        (string_of_bool st.incomplete)
  | Array(t) -> fmt_one "array" (fmt_typ t)
  | ABSTRACT -> "ABSTRACT"



and fmt_typ_list l =
  let typs = List.map fmt_typ l in
  fmt_list typs

let fmt_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let fmt_uop = function 
  Neg -> "-"
| Not -> "!"

let fmt_pop = function
  Inc -> "++"
| Dec -> "--"

let fmt_params l =
  let fmt_p = function
    (t, n) -> String.concat "" ["("; fmt_typ t; ", "; n; ")"] in
  fmt_list (List.map fmt_p l)

let rec fmt_expr = function
  IntLit(l) -> fmt_one "IntLit" (string_of_int l)
| FloatLit(l) -> fmt_one "FloatLit" l
| StrLit(l) -> fmt_one "StrLit"  l
| BoolLit(l) -> fmt_one "BoolLit" (string_of_bool l)
| Id(s) -> fmt_one "Id" s
| Binop(e1, o, e2) -> fmt_three "Binop" (fmt_expr e1) (fmt_op o) (fmt_expr e2)
| Unop(uo, e) -> fmt_two "Unop" (fmt_uop uo) (fmt_expr e)
| Pop(e, po) -> fmt_two "Pop" (fmt_expr e) (fmt_pop po)
| Assign(e1, e2) -> fmt_two "Assign" (fmt_expr e1) (fmt_expr e2)
| ArrayAccess(s, e) -> fmt_two "ArrayAccess" (fmt_expr s) (fmt_expr e)
| Dot(e, s) -> fmt_two "Dot" (fmt_expr e) s
| FCall((* TODO need this? (*n*)*)_, (*a*)_) -> "FCall"
(* below actually is parsed with {name = e.name; param = e.params;
 * typ = e.typ; body = e.body}. See test programs for examples. *)
| FExpr(e) -> fmt_three "FExpr" (fmt_params e.params) 
        (fmt_typ e.typ) (fmt_stmt_list e.body)
| StructInit(l) -> fmt_one "StructInit" (fmt_init l)
| ArrayLit(l) -> fmt_one "ArrayLit" (fmt_list (List.map fmt_expr l))
| New(t) -> fmt_one "New" (fmt_n t)
| Noexpr -> ""
| ArrayLength(e) -> fmt_one "ArrayLength" (fmt_expr e)

and fmt_n = function
  NArray(t, s) -> fmt_two "NArray" (fmt_typ t) (fmt_expr s)
| NStruct(n) -> fmt_one "NStruct" n

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
| Destruct(_, _, _) -> "Destruct"
| Return(e) -> fmt_one "Return" (fmt_expr e)
| VDecl (t, n, l) -> fmt_three "VDecl" (fmt_typ t) n (match l with 
  None -> "" | Some(e) -> fmt_expr e)
| ForLoop (init, e2, e3, s) -> 
  fmt_four "ForLoop" 
  (match init with None -> "" | Some(s) -> fmt_stmt s)
  (fmt_opt_expr e2) 
  (fmt_opt_expr e3) (fmt_stmt_list s)
| WhileLoop (e, s) ->
  fmt_two "WhileLoop" (fmt_opt_expr e) (fmt_stmt_list s)
| StructDef(n, m) -> fmt_two "StructDef" n (fmt_members m)
| EnhancedFor(t, n, e, b) -> fmt_four "EnhancedFor" (fmt_typ t) n 
  (fmt_expr e) (fmt_stmt_list b)
| If(e, tL, fL) -> fmt_three "If" (fmt_expr e) (fmt_stmt_list tL) 
  (fmt_stmt_list fL)

and fmt_stmt_list l =
  let stmts = List.map fmt_stmt l in
  String.concat "\n" stmts

and fmt_opt_expr = function
  None -> ""
| Some(e) -> fmt_expr e

let string_of_program ast =
  fmt_stmt_list ast