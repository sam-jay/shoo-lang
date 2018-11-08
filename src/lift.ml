open Sast
open Ast

module StringMap = Map.Make(String)

(* 
this needs to go into semant.ml once the issues there are cleaned up

| FExpr(fexpr) ->
  let func_t = Func({
    param_typs = List.map fst fexpr.params;
    return_typ = fexpr.typ;
  }) in
  let nctxt = match fexpr.name with
    "" -> ctxt
  | name -> add_to_ctxt (func_t, None) name ctxt in
  let nctxt = (create_scope fexpr.params)::nctxt in
  let (nctxt, t, ssl) = check_stmt_list nctxt fexpr.body in
  if t = fexpr.typ then (List.tl nctxt, (func_t, SFExpr({
    srecursive = false; (* TODO: handle recursion *)
    styp = fexpr.typ;
    sparams = fexpr.params;
    sbody = ssl
  })))
  else raise (Failure "invalid function return type")

*)

type environment = {
  variables: typ StringMap.t;
  parent: environment option;
}

(* Lifted function *)
type lfunc = {
  lname: string;
  lfvs: bind list; (* Free variables *)
  lreturn_typ: typ;
  lparams: bind list;
  lbody: sstmt list;
}

(* For inbuilt functions, create empty function types *)
let built_in_decls =
  let empty_func ty = ({ return_typ = ty; param_typs = [] }) in
  let add_default map (name, ty) = StringMap.add name (Func ty) map in
  let builtins = List.map (fun (name, Func(func_t)) -> (name, empty_func func_t.return_typ)) Semant.builtins in
  List.fold_left add_default StringMap.empty builtins

let rec lookup (e : environment) name =
try
  StringMap.find name e.variables
with Not_found -> match e.parent with
  Some(parent) -> lookup parent name
| None ->
  try StringMap.find name built_in_decls
  with Not_found -> raise (Failure ("Lift: undeclared identifier " ^ name))

let add_bind m (t, id) = StringMap.add id t m

let rec dfs_sstmt funcs env sstmt =
  let (funcs', fvs', env', sstmt') =
  match sstmt with
    SVDecl(lt, name, Some(sexpr)) ->
      let (funcs', fvs', sexpr') = dfs_sexpr funcs env sexpr ~fname:name in
      let (rt, _) = sexpr' in
      let new_typ = match (lt, rt) with
        Func(_), Func(_) -> lt
      | _ -> lt in
      let env' = {
        variables = StringMap.add name new_typ env.variables;
        parent = env.parent
      } in
      (funcs', fvs', env', SVDecl(new_typ, name, Some(sexpr')))
  | SReturn e -> 
      let (funcs1, fvs1, e1) = dfs_sexpr funcs env e in
      (funcs1, fvs1, env, SReturn(e1))
  | SExpr e ->
      let (funcs1, fvs1, e1) = dfs_sexpr funcs env e in
      (funcs1, fvs1, env, SExpr(e1))
  | SForLoop (Some(s1), Some(e1), Some(e2), body) ->
      let (funcs1, fvs1, env', s1') = dfs_sstmt funcs env s1 in
      let (funcs2, fvs2, e1') = dfs_sexpr funcs1 env' e1 in
      let (funcs3, fvs3, e2') = dfs_sexpr funcs2 env' e2 in
      let (funcs4, fvs4, env_body, body') = List.fold_left 
        (fun (funcs, fvslist, env_curr, stmtlist) s ->
          let (funcs_lift, fvs_lift, env_lift, slift) =
          dfs_sstmt funcs env_curr s in 
          (funcs_lift, fvs_lift::fvslist, env_lift, slift::stmtlist)
        ) (funcs3, [fvs3; fvs2; fvs1], env', []) body in
      (funcs4, List.concat (List.rev fvs4), env_body, 
      SForLoop(Some(s1'), Some(e1'), Some(e2'), body'))
  | _ -> print_endline(fmt_sstmt sstmt); raise (Failure "not implemented in lifter") in
  let check_scope (_, fv) = not (StringMap.mem fv env.variables) in
  let fvs' = List.filter check_scope fvs' in
  (funcs', fvs', env', sstmt')

and dfs_sstmts funcs env = function
  [] -> (funcs, [], env, [])
| sstmt :: rest ->
  let (funcs1, fvs1, env1, sstmts1) = dfs_sstmt funcs env sstmt in
  let new_env = {
    variables = List.fold_left add_bind env1.variables fvs1;
    parent = env1.parent
  } in
  let (funcs2, fvs2, env2, sstmts2) = dfs_sstmts funcs1 new_env rest in
  (funcs2, List.concat [fvs1; fvs2], env2, sstmts1::sstmts2)

and dfs_sexpr ?fname funcs env (t, expr) =
  let check_scope (_, fv) = not (StringMap.mem fv env.variables) in
  let (funcs', fvs', expr') = match expr with
    SFExpr(fexpr) ->
    let (funcs', fvs', (t, clsr)) = match fname with
      Some x -> build_closure funcs env fexpr ~fname:x
    | None -> build_closure funcs env fexpr in
    let fvs' = List.filter check_scope fvs' in
    (funcs', fvs', (t, SClosure(clsr)))
  | SId s1 ->
      let fv =
        if StringMap.mem s1 env.variables || StringMap.mem s1 built_in_decls
        then []
        else [lookup env s1, s1]
      in
      (funcs, fv, (t, SId(s1)))
  | SBinop(se1, op, se2) ->
      let (funcs1, fvs1, se1') = dfs_sexpr funcs env se1 in
      let (funcs2, fvs2, se2') = dfs_sexpr funcs1 env se2 in
        (funcs2, List.concat [fvs1; fvs2], (t, SBinop(se1', op, se2')))
  | SFCall((lt, se), args) ->
    (match se with
    SId(s1) ->
      let fv' = 
        if StringMap.mem s1 env.variables || StringMap.mem s1 built_in_decls
        then None
        else Some(lookup env s1, s1)
      in
      let (funcs1, fvs1, args') = dfs_sexprs funcs env args in
      let fvs' = match fv' with
        Some(x) -> x :: fvs1
      | _ -> fvs1
      in (funcs1, fvs', (lt, SFCall((lt, se), args')))
    | _ ->
        let (funcs1, fvs1, se1) = dfs_sexpr funcs env (lt, se) in
        let (funcs2, fvs2, args') = dfs_sexprs funcs1 env args in
        (funcs2, fvs1@fvs2, (t, SFCall((lt, se), args'))))
  | _ as x -> (funcs, [], (t, x))
  in
  let fvs' = List.filter check_scope fvs' in
  (funcs', fvs', expr')

and dfs_sexprs funcs env = function
  [] -> (funcs, [], [])
| sexpr :: rest ->
  let (funcs1, fvs1, sexpr1) = dfs_sexpr funcs env sexpr in
  let new_env = {
    variables = List.fold_left add_bind env.variables fvs1;
    parent = env.parent;
  } in
  let (funcs2, fvs2, rest) = dfs_sexprs funcs1 new_env rest in
  (funcs2, List.concat [fvs1; fvs2], sexpr1 :: rest)

and build_closure ?fname funcs env fexpr =
  let vars = List.fold_left add_bind StringMap.empty fexpr.sparams in
  let name = match fname with Some x -> x | None -> "" in
  let vars_rec = match name with "" -> vars | _ -> StringMap.add name ABSTRACT vars in
  let new_env = {
    variables = vars_rec;
    parent = Some env
  } in
  let (funcs', fvs, _, body') = dfs_sstmts funcs new_env fexpr.sbody in
  let clsr = {
    ind = List.length funcs';
    free_vars = fvs;
  } in
  let new_func = {
    lname = name;
    lfvs = fvs;
    lreturn_typ = fexpr.styp;
    lparams = fexpr.sparams;
    lbody = body'
  } in
  let func_t = {
    param_typs = List.map fst fexpr.sparams;
    return_typ = fexpr.styp
  } in
  (new_func :: funcs', fvs, (Func(func_t), clsr))

(* Lift takes a list of sast stmts, and converts to a list of (fname, func) *)
(* sstmt list -> (string * lfunc) list *)
let lift sstmts =
  let default_env = { variables = StringMap.empty; parent = None } in
  let (funcs, _, _, sstmts') = dfs_sstmts [] default_env sstmts in
  let main_func = {
    lname = "main";
    lfvs = [];
    lreturn_typ = Int;
    lparams = [];
    lbody = sstmts'
  } in
  let name i func = ("f" ^ string_of_int i, func) in
  let named_funcs = List.mapi name (List.rev funcs) in
  (("main", main_func) :: named_funcs)


(*
type lfunc = {
  lname: string;
  lfvs: bind list; (* Free variables *)
  lreturn_typ: typ;
  lparams: bind list;
  lbody: sstmt list;
}*)

let fmt_lfunc f = String.concat "\n" [
  " -fvs: " ^ String.concat "" (List.map (fun (t, n) -> (fmt_typ t) ^ " " ^ n) f.lfvs);
  " -return_t: " ^ fmt_typ f.lreturn_typ;
  " -params: " ^ String.concat "" (List.map (fun (t, n) -> (fmt_typ t) ^ " " ^ n) f.lparams);
  " -lbody: \n" ^ fmt_sstmt_list f.lbody ~spacer:"    ";
]

let helper (name, f) = name ^ ":\n" ^ (fmt_lfunc f)

let rec string_of_lsast = function
  [] -> ""
| item :: rest -> String.concat "\n" [(helper item);(string_of_lsast rest)]
