type size =
  Fixed of int
| Param of string

type typ =
  Any
| Void
| Int
| Float
| Bool
| String
| Func
| Struct of string
| Array of typ * size

type expr =
  IntLit of int
| FloatLit of float
| StrLit of string
| BoolLit of bool
| ArrayLit of expr list
| Id of string
| Assign of string * expr
| FCall of string * expr list
| FExpr of (typ * string) list * typ * stmt list
| StructInit of (string * expr) list
| Destruct of string list * expr
| New of typ
| NoExpr

and stmt =
  Expr of expr
| VDecl of typ * string * expr option
| Return of expr
| FDecl of string * (typ * string) list * typ * stmt list
| ForLoop of expr * expr * expr * stmt list
| StructDef of string * (typ * string * expr option) list
| EnhancedFor of typ * string * expr * stmt list

type program = stmt list
