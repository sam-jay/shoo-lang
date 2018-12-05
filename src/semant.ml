open Ast
open Sast

module StringMap = Map.Make (String)

exception Type_mismatch of string
exception Undeclared_reference of string

(* READ-THIS!!

  ctxt is a list of StringMaps [ StringMap; StringMap; ... ]
  each StringMap is a map from string:v_name to (type, bool)
  where bool indicates whether the var is initialized or not.

  Whenever we enter a new function scope, we append a new StringMap to
  the FRONT of the ctxt list, and take it off the list when we leave that
  scope.

  This way we have a scope object at the head of the list that we add
  new declarations to, which just gets popped off once we're done with that
  scope.

  However, since functions can access variables in outer scopes, we need to
  maintain this "stack" of scopes, and make necessary modifications to the
  outer scopes as we make progress through the program.

 *)

let check_assign lvaluet rvaluet err =
    if lvaluet = rvaluet then lvaluet else raise err

let rec compare_typs t1 t2 = match t1, t2 with
    SStruct(s_l), SStruct(s_r) -> s_l.sstruct_name = s_r.sstruct_name
  | SArray(SAny), SArray(_) -> true
  | SArray(_), SArray(SAny) -> true
  | SFunc(f1), SFunc(f2) ->
      let same_ret = compare_typs f1.sreturn_typ f2.sreturn_typ in
      let same_args = List.for_all2 compare_typs f1.sparam_typs f2.sparam_typs
      in same_ret && same_args
  | _ -> t1 = t2
  
let check_asn lvalue_t rvalue_t =
  let found_match = compare_typs lvalue_t rvalue_t in
  if found_match
  then lvalue_t
  else (*(print_endline(fmt_styp lvalue_t); print_endline(fmt_styp rvalue_t);*)
    raise (Type_mismatch ("type mismatch error " ^ fmt_styp lvalue_t ^ " " ^ fmt_styp rvalue_t))

(* This function takes a tuple with the type and the map 
 * as well as the variable name and the context map.
 * The map in the tuple is used for the member fields
 * in structs. The map is None unless you are adding a new
 * struct type. *)    
let add_to_ctxt (v_type : styp) (v_name : string) 
    (ctxt : styp StringMap.t list) =
  let map = List.hd ctxt in
  try
    match (StringMap.find v_name map) with
    _ -> raise (Failure (v_name ^ " already declared"))
  with Not_found ->
    let newMap = StringMap.add v_name v_type map in
    newMap::List.tl ctxt

(* Returns a tuple with the type and the map and if
 * the variable is initalized or not. The type and
 * map are optional. *)
let rec find_in_ctxt (v_name : string) (ctxt : styp StringMap.t list) =
  try
    StringMap.find v_name (List.hd ctxt)
  with Not_found -> match List.tl ctxt with
    [] -> raise (Undeclared_reference ("undeclared reference " ^ v_name))
  | tail -> find_in_ctxt v_name tail


let context_to_bindings (ctxt : styp StringMap.t list) =
  let combine_scopes s1 s2 = StringMap.union (fun _ v1 _ -> Some(v1)) s1 s2 in
  let map = List.fold_left combine_scopes StringMap.empty ctxt in
  StringMap.bindings map

let rec ignore_structs t = match t with
  SStruct(st) ->
    SStruct({
      sstruct_name = st.sstruct_name;
      smembers = st.smembers;
      sincomplete = st.sincomplete;
      signore = true;
    })
| SArray(t) -> SArray(ignore_structs t)
| _ -> t

(* This functions gives the member map if the 
 * type given is a struct. *)
(*let get_members_if_struct v_type ctxt = match v_type with  
    Struct(struct_name) -> 
            let ((_,m), _) = find_in_ctxt struct_name ctxt
            in 
    (*        (match m with 
            Some(map) -> map
            | None -> None)*)
            m
    (* Not a struct so shouldn't have a members map *)
    | _ -> None   
*)

(* Returns a tuple with a map and another tuple.
 * The second tuple has the type and the stype. *)
let rec check_expr (ctxt : styp StringMap.t list) = function
| IntLit(x) -> (ctxt, (SInt, SIntLit x))
| BoolLit(x) -> (ctxt, (SBool, SBoolLit x))
| FloatLit(x) -> (ctxt, (SFloat, SFloatLit x))
| StrLit(x) -> (ctxt, (SString, SStrLit x))

| New(NStruct(name)) ->
    let t = find_in_ctxt name ctxt in
    (ctxt, (t, SNew(SNStruct(t))))
| New(NArray(t,e)) ->
    let (nctxt, (t_e, se_e)) = check_expr ctxt e 
    in
    if t_e <> SInt then raise (Failure ("array size must be an integer type"))
    else let st = ignore_structs(styp_of_typ ctxt t) in (nctxt, (SArray (st), 
      SNew(SNArray(st, (t_e, se_e)))))

| StructInit(assigns) -> 
    let (struct_t, assigns') = 
      let assigns' = List.map (fun (n, e) -> let (_, se) = check_expr ctxt e 
          in (n, se)) assigns in
      let compare = function
        SStruct(st) when not st.signore ->
          let mems = List.map (fun (n, (t, _)) -> (n, t)) 
            (StringMap.bindings st.smembers) in
          let compare_by (n1, _) (n2, _) = compare n1 n2 in
          let mems = List.sort compare_by mems in
          let assigns' = List.sort compare_by assigns' in
          if List.length mems <> List.length assigns' then false
          else List.for_all2 (fun (n1, t1) (n2, (t2, _)) 
            -> (compare_typs t1 t2) && n1 = n2) mems assigns'
      | _ -> false
      in
      let binds = context_to_bindings ctxt in
      let types = List.map (fun (_, v) -> v) binds in
      let struct_ts = List.filter compare types in
      match struct_ts with
        [] -> raise (Failure "invalid struct initialization")
      | hd::[] -> (hd, assigns')
      | _ -> raise (Failure "shouldn't happen")
    in
    (ctxt, (struct_t, SStructInit(struct_t, assigns')))
    
(* TODO(claire) This doesn't handle arrays of structs I don't think? *)
(* Go through all the items in the square brackets to see if they match *)
(* TODO(claire) check_expr never changes the map so we shouldn't pass
 * it around all the time. Change this so there is a function above that
 * takes the map and then an nest function that does nothing and doesn't
 * take the map. *)
| ArrayLit(x) -> 
    (* TODO(claire) need to check if the list is empty 
     * TODO(claire) actually we shouldn't allow this to be empty because
     * then you don't know the type of the array and that is a big mess.
     * So many in the parser we should add something to reject if they
     * try to use [] to init an array and [] is empty. They should just
     * used new instead. *)
    if List.length x = 0
        then raise (Failure "empty array init is not supported")
    else 
        let (_, (item_type, _)) = check_expr ctxt (List.hd x) in
        let item_type = ignore_structs item_type in (* recursively do this everywhere *)
        let t = List.map (fun e1 ->
            let (_, (t1, st1)) = check_expr ctxt e1 in
            (* TODO(claire) need to check both? *)
            if (t1 = item_type) (*&& (st1 = item_s_type)*) then (t1, st1)
            else raise (Failure("Error: cannot have multiple types in an array ("
              ^ fmt_styp t1 ^ " and " ^ fmt_styp item_type ))
        ) x in (ctxt, (SArray(item_type), SArrayLit t))

| ArrayAccess(expr, int_expr) ->
    let (_, (t1, se1)) = check_expr ctxt expr in
    let (_, (t2, se2)) = check_expr ctxt int_expr in
    let t3 = match t1 with 
      SArray(t) -> t 
      | _ -> raise (Failure ("not an array"))
    in
    if t2 = SInt then (ctxt, (t3, SArrayAccess((t1, se1), (t2, se2))))
    else raise (Failure ("can't access array with non-integer type"))

| Id(n) -> 
    let t = find_in_ctxt n ctxt in
    (ctxt, (t, SId n))

| Assign(e1, e2) ->
    let (_, (t1, se1)) = match e1 with
        Id(n) -> let t = find_in_ctxt n ctxt in
                (ctxt, (t, SId n))
      | _ -> check_expr ctxt e1 
    in
    let (_, (t2, se2)) = check_expr ctxt e2 in
    (ctxt, (check_asn t1 t2, SAssign((t1, se1), (t2, se2))))

| Dot(e, field_name) -> 
    let check_struct_access struct_type field_name = 
      let ((access_type, _), st) = (match struct_type with
        (* make sure you were passed a struct *)
        SStruct(s) ->
          (let t = find_in_ctxt s.sstruct_name ctxt in 
            (* Get the complete struct with members *)
          let struct_t = match t with 
            SStruct(st) -> st 
            | _ -> raise (Failure "not a struct")
          in
          try (StringMap.find field_name struct_t.smembers, SStruct(struct_t))
          with Not_found -> 
            raise (Failure 
              ("struct " ^ s.sstruct_name ^ " has no member " ^ field_name)))
        | _ -> print_endline(fmt_styp struct_type); 
          raise (Failure "dot operator used on non-struct type"))
      in (access_type, st)
    in
    let (_, (t1, se1)) = check_expr ctxt e in
    let (field_type, _ ) = check_struct_access t1 field_name 
    in (ctxt, (field_type, SDot((t1, se1), field_name)))

| Binop(e1, op, e2) ->
    let (nctxt, (lt, se1)) = check_expr ctxt e1 in
    let (nctxt, (rt, se2)) = check_expr nctxt e2 in
    let sbinop = SBinop((lt, se1), op, (rt, se2)) in
    (match op with
      Add | Sub | Mult | Div | Mod when lt = SInt && rt = SInt 
        -> (nctxt, (SInt, sbinop))
      | Add | Sub | Mult | Div when lt = SFloat && rt = SFloat 
        -> (nctxt, (SFloat, sbinop))
      (* allow string concatenation TODO(crystal): update LRM *)
      | Add when lt = SString && rt = SString -> (nctxt, (SString,sbinop))
     (* TODO(claire): make sure LRM says that we can compare all
      * expressions of the same type using ==, including functions, 
      * strings,
      * structs, arrays? *)
      | Equal | Neq  when lt = rt -> (nctxt, (SBool, sbinop))
      | Less | Leq | Greater | Geq  
                              when (lt = SInt && rt = SInt) 
                              || (lt = SFloat || rt = SFloat) -> 
                                      (nctxt, (SBool, sbinop))
      | And | Or when lt = SBool && rt = SBool -> (nctxt, (SBool, sbinop))
      | _ -> raise (Failure("Error: cannot use " ^ fmt_op op ^ 
        " with types: "^ fmt_styp rt ^ " and " ^ fmt_styp lt )))

| Unop(op, e) -> 
    let (nctxt, (t, e)) = check_expr ctxt e in 
    let sunop = SUnop(op, (t, e)) in
    (match op with 
      Neg when t = SInt -> (nctxt, (SInt, sunop))
    | Neg when t = SFloat -> (nctxt, (SFloat, sunop))
    | Not when t = SBool -> (nctxt, (SBool, sunop))
    | _ -> raise (Type_mismatch "Type mismatch for unary operator"))

| Pop(e, op) ->
    let (nctxt, (t, e)) = check_expr ctxt e in 
    let spop = SPop((t, e), op) in
    (match op with 
      Inc when t = SInt -> (nctxt, (SInt, spop))
    | Dec when t = SInt -> (nctxt, (SInt, spop))
    | _ -> raise (Type_mismatch "Type mismatch for unary operator"))

| FCall(expr, args) ->
    let check_args f_type args =
      let rec helper sl = function
        ([], []) -> sl
      | (p_typ::pl, arg::al) ->
        let (_, (a_typ, se)) = check_expr ctxt arg in
        if compare_typs p_typ a_typ then helper ((a_typ, se)::sl) (pl, al)
        else raise (Failure "argument type mismatch")
      | _ -> raise (Failure "invalid number of arguments")
      in
      helper [] (f_type.sparam_typs, args)
    in
    let (_, (t, se)) = check_expr ctxt expr in
    let (func_t, sl) = match t with
      SFunc(func_t) -> (func_t, check_args func_t args)
    | _ -> raise (Failure "not a function")
    in
    (ctxt, (func_t.sreturn_typ, SFCall((t, se), sl)))

| FExpr(fexpr) ->
    let conv_params (typ, _ ) = (styp_of_typ ctxt typ) in
    let conv_params_with_both_fields (typ, str) = (styp_of_typ ctxt typ,str) in
    let sfunc_t = SFunc({ sreturn_typ = styp_of_typ ctxt fexpr.typ; sparam_typs = List.map conv_params fexpr.params }) in
    let create_scope list =
      let rec helper m = function
        [] -> m
      | (t, n)::tl -> 
          let new_m = StringMap.add n (styp_of_typ ctxt t) m in 
              helper new_m tl
      in
      if fexpr.name <> ""
      then helper (StringMap.add fexpr.name sfunc_t StringMap.empty) list
      else helper StringMap.empty list
    in
    let func_scope = create_scope fexpr.params in
    let (_, return_t, sl) = check_stmt_list (func_scope::ctxt) fexpr.body in
    ignore (check_asn return_t (styp_of_typ ctxt fexpr.typ)); (*TODO: ignore this output to get rid of warnings*)
    (ctxt, (sfunc_t, SFExpr({
      sname = fexpr.name;
      styp = styp_of_typ ctxt fexpr.typ;
      sparams = List.map conv_params_with_both_fields fexpr.params;
      sbody = sl;
      srecursive = false; (* TODO: handle recursion *)
    })))

| Noexpr -> (ctxt, (SVoid, SNoexpr))
| _ as x -> print_endline(Ast.fmt_expr x); raise (Failure "not implemented in semant")

and styp_of_typ ctxt = function
    Int -> SInt
  | Bool -> SBool
  | Float -> SFloat
  | String -> SString
  | Void -> SVoid
  | Func f -> SFunc({ sparam_typs = List.map (styp_of_typ ctxt) f.param_typs; sreturn_typ = styp_of_typ ctxt f.return_typ })
  | Struct s -> find_in_ctxt s.struct_name ctxt
  | Array t -> SArray(styp_of_typ ctxt t) 
  | ABSTRACT -> SABSTRACT
  (*| _ as x -> raise (Failure ("TODO NEED TO convert this typ to styp: " ^ (fmt_typ x)))*)

and check_stmt_list (ctxt : styp StringMap.t list) = function
  [] -> (ctxt, SVoid, [])
| hd::tl -> 
  let (nctxt, t, ss) = check_stmt ctxt hd in
  let (nctxt, t_rest, ssl) = check_stmt_list nctxt tl in
  let ret =
    if t = SVoid
    then t_rest 
    else (if List.length tl <> 0 then raise (Failure "dead code after return") else (); t)
  in
  (nctxt, ret, ss::ssl) (* returned something *)

and check_bool_expr (ctxt : styp StringMap.t list) e = 
    let (nctxt, (t, st)) = check_expr ctxt e in
    if (t <> SBool) then 
      raise (Failure("Error: " ^ fmt_styp t ^ " is not a boolean type"))
    else (nctxt, (t, st)) 

(* returns the map, type, stype *)
and check_stmt (ctxt : styp StringMap.t list) = function
  Expr(e) -> let (nctxt, (t, ss)) = 
      check_expr ctxt e in (nctxt, SVoid, SExpr((t, ss)))
| VDecl(ltype, n, i) ->
  let t = match ltype with 
    Struct(struct_t) -> find_in_ctxt struct_t.struct_name ctxt 
    | _ -> styp_of_typ ctxt ltype
  in
  let t = ignore_structs t in
  (match i with
    None -> (add_to_ctxt t n ctxt, SVoid, SVDecl(t, n, None))
  | Some(e) ->
      let (_, (t_i, s_i)) = check_expr ctxt e in
      let nctxt = add_to_ctxt t n ctxt in
      (nctxt, SVoid, SVDecl((check_asn t_i t), n, Some((t_i, s_i)))))

| StructDef(name, fields) ->
    let conv_typ typ = styp_of_typ ctxt typ in
    let helper (map, list) (lt, n, i) =
      let lt = conv_typ lt in
      match lt with
        SStruct(struct_t) when name = struct_t.sstruct_name -> raise (Failure ("illegal recursive struct " ^ name))
      | _ ->
      try
      match (StringMap.find n map) with
        _ -> raise (Failure (n ^ " already declared in struct " ^ name))
      with Not_found ->
        let (init, opt_se) = match i with
          None -> (None, None)
        | Some e ->
          let (_, (rt, se)) = check_expr ctxt e in
          let _ = check_asn lt rt in
          (Some(rt, se), Some(rt, se)) (*this was changed to make it compile...idk if it's right*)
        in
        let new_map = StringMap.add n (lt, init) map in
        let new_list = (lt, n, opt_se)::list in
        (new_map, new_list)
    in
    let (members, fields_se) = List.fold_left helper (StringMap.empty, []) fields in
    let struct_t = SStruct({
      sstruct_name = name;
      smembers = members;
      sincomplete = false;
      signore = false;
    }) in
    let nctxt = add_to_ctxt struct_t name ctxt in
    (nctxt, SVoid, SStructDef(name, fields_se))

| Return(e) -> 
  let (nctxt, (t, ss)) = check_expr ctxt e in 
    (nctxt, t, SReturn((t, ss)))
| ForLoop (s1, e2, e3, st) -> 
     let (ctxt1, s1') = match s1 with
        None -> (ctxt, None)
        | Some(s1) -> (let (nctxt, _, ns1) = check_stmt ctxt s1 in
            (nctxt, Some(ns1)))
     in
     let (ctxt2, e2') = match e2 with
        None -> (ctxt1, None)
        | Some(e2) -> (let (nctxt, (t_i, si)) = 
            check_bool_expr ctxt1 e2 in (nctxt, Some((t_i, si))))
     in
     let (ctxt3, e3') = match e3 with
        None -> (ctxt2, None)
        | Some(e3) -> (let (nctxt, (t_i, si)) = 
            check_expr ctxt2 e3 in (nctxt, Some((t_i, si))))
     in
     let (ctxt4, ret_t, st') = check_stmt_list ctxt3 st
     in
    (ctxt4, ret_t, SForLoop(s1', e2', e3', st'))

(* Note: Handling the context variable of two branches is kinda tricky because
   it does not follow a linear flow. My assumption is that everything 
   defined in the block should not be effective outside of the if block
   and that it should be consistent between For and If. *)
| If (e, st1, st2) ->
    let (ctxt1, e') = check_bool_expr ctxt e
    in
    let (_, rt1, st1') = check_stmt_list ctxt1 st1
    in
    let (_, rt2, st2') = check_stmt_list ctxt1 st2
    in
    let rt = match rt1, rt2 with
      (SVoid, _) -> SVoid
    | (_, SVoid) -> SVoid
    | (t1, t2) -> check_asn t1 t2
    in
    (ctxt, rt, SIf(e', st1', st2'))
    
| _ -> (ctxt, SVoid, SExpr((SVoid, SNoexpr)))

let builtins = [
  ("println", SFunc({ sparam_typs = [SString]; sreturn_typ = SVoid }));
  ("print", SFunc({ sparam_typs = [SString]; sreturn_typ = SVoid }));
  ("str_of_int", SFunc({ sparam_typs = [SInt]; sreturn_typ = SString }));
  ("str_of_bool", SFunc({ sparam_typs = [SBool]; sreturn_typ = SString }));
  ("string_concat", SFunc({ sparam_typs = [SString; SString]; sreturn_typ = SString })); 
  ("string_equals", SFunc({ sparam_typs = [SString; SString]; sreturn_typ = SInt })); 
  ("length", SFunc({ sparam_typs = [SArray(SAny)]; sreturn_typ = SInt })); 
  ("str_of_float", SFunc({ sparam_typs = [SFloat]; sreturn_typ = SString })); 
  ("int_of_float", SFunc({ sparam_typs = [SFloat]; sreturn_typ = SInt })); 
  ("float_of_int", SFunc({ sparam_typs = [SInt]; sreturn_typ = SFloat })); 
  ("scan_line", SFunc({ sparam_typs = [SInt]; sreturn_typ = SString })); 
  ("exit_success", SFunc({ sparam_typs = [SInt]; sreturn_typ = SInt })); 
  ("die", SFunc({ sparam_typs = [SString; SInt]; sreturn_typ = SInt })); 
]

let def_ctxt =
  let add_func ctxt (name, func_t) = add_to_ctxt func_t name ctxt in
  List.fold_left add_func [StringMap.empty] builtins

let check_program (prog : stmt list) =
  if List.exists (fun x -> match x with Return(_) -> true | _ -> false) prog
  then raise (Failure "illegal return statement")
  else
    let (_, _, ssl) = check_stmt_list def_ctxt prog in
    ssl

