
type typ =
  Any
| Void
| Int
| Float
| Bool
| String
| Func

type param = typ * string

type expr =
  IntLit of int
| FloatLit of float
| StrLit of string
| BoolLit of bool
| Id of string
| Assign of string * expr
| FCall of string * expr list
| FExpr of param list * typ * stmt list

and stmt =
  Expr of expr
| Return of expr
| FDecl of string * param list * typ * stmt list

type program = stmt list