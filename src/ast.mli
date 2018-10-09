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
| Array of typ

type op = 
  Add 
| Sub 
| Mult 
| Div 
| Equal
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

type newable =
  NArray of typ * expr
| NStruct of string

and expr =
  IntLit of int
| FloatLit of float
| StrLit of string
| BoolLit of bool
| ArrayLit of expr list
| Id of string
| Binop of expr * op * expr
| Unop of uop * expr
| Assign of string * expr
| FCall of string * expr list
| FExpr of (typ * string) list * typ * stmt list
| StructInit of (string * expr) list
| Destruct of string list * expr
| New of newable

and stmt =
  Expr of expr
| VDecl of typ * string * expr option
| Return of expr
| FDecl of string * (typ * string) list * typ * stmt list
| If of expr * stmt list * stmt list
| ForLoop of (stmt option) * (expr option) * (expr option) * stmt list
| StructDef of string * (typ * string * expr option) list
| EnhancedFor of typ * string * expr * stmt list

type program = stmt list
