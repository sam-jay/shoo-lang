open Ast


type primitiveType =
  TInt
| TFloat
| TString
| TBool
| TPlaceholder
| TFun of primitiveType * primitiveType
| T of string

type aexpr =
  AIntLit of int * primitiveType
| AFloatLit of float * primitiveType
| AStrLit of string * primitiveType
| ABoolLit of bool * primitiveType
| APlaceholder of int * primitiveType
| AFun of id * aexpr * primitiveType
| AVal of string * primitiveType

module NameMap = Map.Make(String)
type environment = primitiveType NameMap.t

type id = string

type substitutions = (id * primitiveType) list

let type_variable = ref (Char.code 'a')

let gen_new_type () =
  let c1 = !type_variable in
  incr type_variable; T(Char.escaped (Char.chr c1))
;;

let rec annotate_expr (e: expr) (env: environment) : aexpr =
  match e with
    IntLit(n) -> AIntLit(n, TInt)
  | FloatLit(n) -> AFloatLit(n, TFloat)
  | StrLit(s) -> AStrLit(s, TString)
  | BoolLit(b) -> ABoolLit(b, TBool)
  | Id(x) -> 
    if NameMap.mem x env
    then AVal(x, NameMap.find x env)
    else raise (failwith "variable not defined")
  | _ -> APlaceholder(0, TPlaceholder) (* TODO: fix this *)

let rec annotate_stmt (s: stmt) (env: environment) : aexpr =
  match s with
    Expr(e) -> annotate_expr e env
  | _ -> APlaceholder(0, TPlaceholder)

let type_of (ae: aexpr): primitiveType =
  match ae with
    AIntLit(_, t) | AFloatLit(_, t) | AStrLit(_, t) | ABoolLit(_, t) -> t
  | APlaceholder(_, t) -> t
  | AVal(_, t) -> t

let rec collect_expr (ae: aexpr) : (primitiveType * primitiveType) list =
  match ae with
    AIntLit(_) | AFloatLit(_) | AStrLit(_) | ABoolLit(_) -> []
  | AVal(_) -> []
  | _ -> []

let rec substitute (u: primitiveType) (x: id) (t: primitiveType) : primitiveType =
  match t with
  | TInt | TFloat | TString | TBool -> t
  | T(c) -> if c = x then u else t
  | _ -> t

let apply (subs: substitutions) (t: primitiveType) : primitiveType =
  List.fold_right (fun (x, u) t -> substitute u x t) subs t
;;

let rec unify (constraints: (primitiveType * primitiveType) list) : substitutions =
  match constraints with
  | [] -> []
  | (x, y) :: xs ->
    (* generate substitutions of the rest of the list *)
    let t2 = unify xs in
    (* resolve the LHS and RHS of the constraints from the previous substitutions *)
    let t1 = unify_one (apply t2 x) (apply t2 y) in
    t1 @ t2

and unify_one (t1: primitiveType) (t2: primitiveType) : substitutions =
  match t1, t2 with
    TInt, TInt | TFloat, TFloat | TString, TString | TBool, TBool -> []
  | TPlaceholder, TPlaceholder -> []
  | T(x), z | z, T(x) -> [(x, z)]
  | _ -> raise (failwith "mismatched types")
;;

let rec apply_expr (subs: substitutions) (ae: aexpr): aexpr =
  match ae with
    AIntLit(n, t) -> AIntLit(n, apply subs t)
  | AFloatLit(n, t) -> AFloatLit(n, apply subs t)
  | AStrLit(n, t) -> AStrLit(n, apply subs t)
  | ABoolLit(b, t) -> ABoolLit(b, apply subs t)
  | AVal(s, t) -> AVal(s, apply subs t)
  | APlaceholder(s, t) -> APlaceholder(s, apply subs t)
;;

let infer (env: environment) (s: stmt) : aexpr =
  let annotated_stmt = annotate_stmt s env in
  let constraints = collect_expr annotated_stmt in
  let subs = unify constraints in
  type_variable := (Char.code 'a');
  apply_expr subs annotated_stmt
;;

let rec get_ids (s: stmt): id list =
  let rec dedup = function
  | [] -> []
  | x :: y :: xs when x = y -> y :: dedup xs
  | x :: xs -> x :: dedup xs in
  let ids = match s with
    Expr(e) -> (match e with
    | IntLit(_) | FloatLit(_) | StrLit(_) | BoolLit(_) -> []
    | Id(x) -> [x]
    | _ -> []
    )
  | _ -> [] in
  dedup ids
;;

let inf (s: Ast.stmt) : aexpr =
  let vals = get_ids s in
  let env = List.fold_left (fun m x -> NameMap.add x (gen_new_type ()) m) NameMap.empty vals in
  infer env s
;;

module CharMap = Map.Make(String)
type genericMap = int CharMap.t
let string_of_type (t: primitiveType) =
  let rec aux (t: primitiveType) (chr: int) (map: genericMap) =
    match t with
    | TInt -> "int", chr, map
    | TFloat -> "float", chr, map
    | TString -> "string", chr, map
    | TBool -> "bool", chr, map
    | TPlaceholder -> "placeholder", chr, map
    | T(x) ->
      let gen_chr, new_chr, new_map = if CharMap.mem x map
        then Char.escaped (Char.chr (CharMap.find x map)), chr, map
        else
          let c = Char.escaped (Char.chr chr) in
          c, (chr + 1), CharMap.add x chr map
      in
      Printf.sprintf "'%s" gen_chr, new_chr, new_map
    | TFun(t1, t2) -> let (st1, c1, m1) = aux t1 chr map in
      let (st2, c2, m2) = aux t2 c1 m1 in
      (Printf.sprintf "(%s -> %s)" st1 st2), c2, m2 in
  let s, _, _ = aux t 97 CharMap.empty in s
;;

let print s =
  let aexpr = inf s in
  print_endline (string_of_type (type_of aexpr))

let rec exec_stmts = function
   [] -> ()
  | s::tl -> print s; exec_stmts tl

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  exec_stmts program