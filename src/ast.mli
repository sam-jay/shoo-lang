
type typ = Int | Float | Bool | String

type param = typ * string

type expr =
  IntLit of int
| FloatLit of float
| StrLit of string
| BoolLit of bool
| Id of string
| Assign of string * expr
| FDecl of string * param list
| FCall of string * expr list

type stmt =
  Block of stmt list
| Expr of expr

type program = stmt list