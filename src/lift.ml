open Sast

module StringMap = Map.Make(String)

type environment = {
  variables: styp StringMap.t;
  parent: environment option;
}

(* Lifted function *)
type lfunc = {
  lname: string;
  lfvs: sbind list; (* Free variables *)
  lreturn_typ: styp;
  lparams: sbind list;
  lbody: sstmt list;
}

(* For inbuilt functions, create empty function types *)
let built_in_decls =
  let empty_func ty = ({ sreturn_typ = ty; sparam_typs = []; sbuiltin = true; }) in
  let add_default map (name, ty) = StringMap.add name (SFunc ty) map in
  let builtins = List.map (fun (name, func_t) -> 
      let is_func = match func_t with 
          SFunc(func_t) -> (name, empty_func func_t.sreturn_typ) 
        | _ -> raise (Failure ("not a built-in function")) in is_func)
      Semant.builtins in
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
      SStructDef(name, members) ->
      let helper (funcs, fvs, members) (ty, n, sexp_opt) = match sexp_opt with
          None -> (funcs, fvs, (ty, n, None)::members)
        | Some(e) ->
          let (funcs', fvs', e') = dfs_sexpr funcs env e in
          (funcs', fvs@fvs', (ty, n, Some(e'))::members)
      in
      let (funcs', fvs', members') = List.fold_left helper (funcs, [], []) members in
      let new_typ = SStruct({
          sstruct_name = name;
          smembers = List.fold_left (fun m (t, n, e) -> StringMap.add n (t, e) m) StringMap.empty members';
          sincomplete = false;
          signore = false
        }) in
      let env' = {
        variables = StringMap.add name new_typ env.variables;
        parent = env.parent
      } in
      (funcs', fvs', env', SStructDef(name, members'))
    | SVDecl(lt, name, i) ->
      let (new_typ, funcs', fvs', opt_sexpr') = match i with
          None -> (lt, funcs, [], None)
        | Some(sexpr) ->
          let (funcs', fvs', sexpr') = dfs_sexpr funcs env sexpr ~fname:name in
          let (rt, _) = sexpr' in
          let new_typ = match (lt, rt) with
              SFunc(_), SFunc(_) -> lt
            | _ -> lt in
          (new_typ, funcs', fvs', Some(sexpr'))
      in
      let env' = {
        variables = StringMap.add name new_typ env.variables;
        parent = env.parent
      } in
      (funcs', fvs', env', SVDecl(new_typ, name, opt_sexpr'))
    | SReturn e -> 
      let (funcs1, fvs1, e1) = dfs_sexpr funcs env e in
      (funcs1, fvs1, env, SReturn(e1))
    | SIf(e, s1, s2) ->
      let (funcs1, fvs1, e') = dfs_sexpr funcs env e in
      let (funcs2, fvs2, _, s1') = dfs_sstmts funcs1 env s1 in
      let (funcs3, fvs3, _, s2') = dfs_sstmts funcs2 env s2 in
      (funcs3, List.concat [fvs1; fvs2; fvs3], env, SIf(e', s1', s2'))
    | SExpr e ->
      let (funcs1, fvs1, e1) = dfs_sexpr funcs env e in
      (funcs1, fvs1, env, SExpr(e1))
    | SForLoop (s1, e1, e2, body) ->
      let (funcs1, fvs1, env', s1') = match s1 with 
          Some(s1) -> (let (funcs1', fvs1', env'', s1'') = 
                         dfs_sstmt funcs env s1 in 
                       (funcs1', fvs1', env'', Some(s1'')))
        | None -> (funcs, [], env, None) 
      in
      let (funcs2, fvs2, e1') = match e1 with
          Some(e1) -> (let (funcs2', fvs2', e1'') = 
                         dfs_sexpr funcs1 env' e1 in (funcs2',fvs2', Some(e1'')))
        | None -> (funcs1, [], None)
      in
      let (funcs3, fvs3, e2') = match e2 with
          Some(e2) -> (let (funcs3', fvs3', e2'') = 
                         dfs_sexpr funcs2 env' e2 in (funcs3', fvs3', Some(e2'')))
        | None -> (funcs2, [], None)
      in
      let (funcs4, fvs4, env_body, body') = 
        List.fold_left 
          (fun (funcs_curr, fvslist, env_curr, stmtlist) s ->
             let (funcs_lift, fvs_lift, env_lift, slift) =
               dfs_sstmt funcs_curr env_curr s in 
             (funcs_lift, fvs_lift::fvslist, env_lift, slift::stmtlist)
          ) (funcs3, [fvs3; fvs2; fvs1], env', []) body in
      (funcs4, List.concat (List.rev fvs4), env_body, 
       SForLoop(s1', e1', e2', body'))   
    | _ -> print_endline(fmt_sstmt sstmt); raise (Failure "not implemented in lifter") in
  let check_scope (_, fv) = not (StringMap.mem fv env.variables) in
  let fvs' = List.filter check_scope fvs' in
  (funcs', fvs', env', sstmt')

and dfs_sstmts funcs env = function
    [] -> (funcs, [], env, [])
  | sstmt :: rest ->
    let (funcs1, fvs1, env1, sstmts1) = 
      dfs_sstmt funcs env sstmt in
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
    | SNew(SNStruct(SStruct(struct_t))) ->
      let sstruct = lookup env struct_t.sstruct_name in
      (funcs, [], (sstruct, SNew(SNStruct(sstruct))))
    | SStructInit(ty, assigns) ->
      let helper (funcs, fvs, assigns) (n, se) =
        let (funcs', fvs', se') = dfs_sexpr funcs env se in
        (funcs', fvs@fvs', (n, se')::assigns)
      in
      let (funcs', fvs', assigns') = List.fold_left helper (funcs, [], []) assigns in
      (funcs', fvs', (t, SStructInit(ty, assigns')))
    | SDot(se1, field_name) ->
      let (funcs', fvs', expr') = dfs_sexpr funcs env se1 in
      (funcs', fvs', (t, SDot(expr', field_name))) 
    | SArrayLit(sexpr_list) ->
      let helper (funcs, fvs, sexpr_list) se =
        let (funcs', fvs', se') = dfs_sexpr funcs env se in
        (funcs', fvs@fvs', se'::sexpr_list)
      in
      let (funcs', fvs', sexpr_list') = 
        List.fold_left helper (funcs, [], []) sexpr_list in
      (funcs', fvs', (t, SArrayLit(List.rev sexpr_list')))
    | SArrayAccess(expr, int_expr) ->
      let (funcs', fvs2, expr') = dfs_sexpr funcs env expr in
      let (funcs', fvs1, int_expr') = dfs_sexpr funcs' env int_expr in
      (funcs', List.concat [fvs2; fvs1], (t, SArrayAccess(expr', int_expr')))
    | SNew(SNArray(st, expr)) ->
      let (funcs', fvs1, expr') = dfs_sexpr funcs env expr in
      (funcs', fvs1, (t, SNew(SNArray(st, expr'))))
    | SAssign(e1, e2) ->
      let (funcs', fvs2, e2') = dfs_sexpr funcs env e2 in
      let (funcs', fvs1, e1') = dfs_sexpr funcs' env e1 in
      (funcs', List.concat [fvs2; fvs1], (t, SAssign(e1', e2')))
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
    | SUnop(op, e1) ->
      let (funcs1, fvs1, e1') = dfs_sexpr funcs env e1 in
      (funcs1, fvs1, (t, SUnop(op, e1')))
    | SPop(e1, op) ->
      let (funcs1, fvs1, e1') = dfs_sexpr funcs env e1 in
      (funcs1, fvs1, (t, SPop(e1', op)))
    | SFCall((lt, se), args) ->
      (match se with
         SId(s1) ->
         let fv' = 
           if StringMap.mem s1 env.variables || StringMap.mem s1 built_in_decls
           then None
           else Some(lookup env s1, s1)
         in
         let (funcs1, fvs1, args') = dfs_sexprs funcs env (List.rev args) in
         let fvs' = match fv' with
             Some(x) -> x :: fvs1
           | _ -> fvs1
         in (funcs1, fvs', (t, SFCall((lt, se), args')))
       | _ ->
         (* Need this for recursion. *)
         let (funcs1, fvs1, _) 
           = dfs_sexpr funcs env (lt, se) in
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
  let vars_rec = match name with "" -> vars | _ -> StringMap.add name SABSTRACT vars in
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
    sparam_typs = List.map fst fexpr.sparams;
    sreturn_typ = fexpr.styp;
    sbuiltin = false;
  } in
  (new_func :: funcs', fvs, (SFunc(func_t), clsr))

(* Lift takes a list of sast stmts, and converts to a list of (fname, func) *)
(* sstmt list -> (string * lfunc) list *)
let lift sstmts =
  let default_env = { variables = StringMap.empty; parent = None } in
  let (funcs, _, _, sstmts') = dfs_sstmts [] default_env sstmts in
  let main_func = {
    lname = "main";
    lfvs = [];
    lreturn_typ = SInt;
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
    " -fvs: " ^ String.concat "" (List.map (fun (t, n) -> (fmt_styp t) ^ " " ^ n) f.lfvs);
    " -return_t: " ^ fmt_styp f.lreturn_typ;
    " -params: " ^ String.concat "" (List.map (fun (t, n) -> (fmt_styp t) ^ " " ^ n) f.lparams);
    " -lbody: \n" ^ fmt_sstmt_list f.lbody ~spacer:"    ";
  ]

let helper (name, f) = name ^ ":\n" ^ (fmt_lfunc f)

let rec string_of_lsast = function
    [] -> ""
  | item :: rest -> String.concat "\n" [(helper item);(string_of_lsast rest)]
