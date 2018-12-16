open Ast
open Sast

module StringMap = Map.Make (String)

exception Type_mismatch of string
exception Undeclared_reference of string

let builtinsFunc = [
  ("println", Func({ param_typs = [String]; return_typ = Void }));
  ("print", Func({ param_typs = [String]; return_typ = Void }));
  ("str_of_int", Func({ param_typs = [Int]; return_typ = String }));
  ("str_of_bool", Func({ param_typs = [Bool]; return_typ = String }));
  ("string_concat", Func({ param_typs = [String; String]; return_typ = String })); 
  ("string_equals", Func({ param_typs = [String; String]; return_typ = Int })); 
  ("str_of_float", Func({ param_typs = [Float]; return_typ = String })); 
  ("int_of_float", Func({ param_typs = [Float]; return_typ = Int })); 
  ("float_of_int", Func({ param_typs = [Int]; return_typ = Float })); 
  ("scan_line", Func({ param_typs = [Int]; return_typ = String })); 
  ("exit_success", Func({ param_typs = []; return_typ = Void })); 
  ("die", Func({ param_typs = [String; Int]; return_typ = Void })); 
  ("int_of_str", Func({ param_typs = [String]; return_typ = Int })); 
  ("rand_autoseed", Func({ param_typs = []; return_typ = Void }));  
  ("rand_afterseed", Func({ param_typs = []; return_typ = Int }));
  ("scan_char", Func({ param_typs = []; return_typ = String }));
]

let makeMapFromBuiltinsFunc map arrElem = 
  StringMap.add (fst arrElem) (snd arrElem) map

let builtinMap = List.fold_left 
  makeMapFromBuiltinsFunc StringMap.empty builtinsFunc

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
  else
    (print_endline(Printexc.raw_backtrace_to_string(Printexc.get_callstack 100));
     raise (Type_mismatch ("type mismatch error " ^ 
        fmt_styp lvalue_t ^ " " ^ fmt_styp rvalue_t)))

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
  | SFunc(f) -> SFunc({
      sparam_typs = List.map ignore_structs f.sparam_typs;
      sreturn_typ = ignore_structs f.sreturn_typ;
      sbuiltin = f.sbuiltin;
    })
| _ -> t

(* Returns a tuple with a map and another tuple.
 * The second tuple has the type and the stype. *)
let rec check_expr (ctxt : styp StringMap.t list) = function
  | IntLit(x) -> (SInt, SIntLit x)
  | BoolLit(x) -> (SBool, SBoolLit x)
  | FloatLit(x) -> (SFloat, SFloatLit x)
  | StrLit(x) -> (SString, SStrLit x)

  | New(NStruct(name)) ->
    let t = find_in_ctxt name ctxt in
    (t, SNew(SNStruct(t)))
  | New(NArray(t,e)) ->
    let (t_e, se_e) = check_expr ctxt e 
    in
    if t_e <> SInt then raise (Failure ("array size must be an integer type"))
    else let st = ignore_structs(styp_of_typ ctxt t) in (SArray (st), 
                                        SNew(SNArray(st, (t_e, se_e))))
  | StructInit(assigns) -> 
    let (struct_t, assigns') = 
      let assigns' = List.map (fun (n, e) -> let se = check_expr ctxt e 
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
    (struct_t, SStructInit(struct_t, assigns'))

  (* Go through all the items in the square brackets to see if they match *)
  (* check_expr never changes the map so we shouldn't pass
   * it around all the time. *) 
  | ArrayLit(x) -> 
    if List.length x = 0
    then raise (Failure "empty array init is not supported")
    else 
      let (item_type, _) = check_expr ctxt (List.hd x) in
      let item_type = ignore_structs item_type in 
        (* recursively do this everywhere *)
      let t = List.map (fun e1 ->
          let (t1, st1) = check_expr ctxt e1 in
          let t1 = ignore_structs t1 in
          if (t1 = item_type) then (t1, st1)
          else raise (Failure("Error: cannot have multiple types in an array ("
                              ^ fmt_styp t1 ^ " and " ^ fmt_styp item_type ))
        ) x in (SArray(item_type), SArrayLit t)
  
  | ArrayAccess(expr, int_expr) ->
    let (t1, se1) = check_expr ctxt expr in
    let (t2, se2) = check_expr ctxt int_expr in
    let t3 = match t1 with 
        SArray(t) -> t 
      | _ -> raise (Failure ("not an array"))
    in
    if t2 = SInt then (t3, SArrayAccess((t1, se1), (t2, se2)))
    else raise (Failure ("can't access array with non-integer type"))

  | Id(n) -> 
    let t = find_in_ctxt (if String.contains n '~' then String.sub n 1 
        ((String.length n) - 1) else n) ctxt in
    if (String.contains n '~') then 
        (t, SId (String.sub n 1 ((String.length n) - 1)))
    else (match t with
      SFunc(f) when f.sbuiltin -> 
        let ft = match StringMap.find n builtinMap with 
            Func(func) -> func | _ -> raise (Failure ("shouldn't happen")) in
      check_expr ctxt (FExpr({
      name = "";
      typ = ft.return_typ;
      params = List.mapi (fun i t -> (t, "__p" ^ (string_of_int i))) ft.param_typs;
      body = if ft.return_typ == Void then [
        Expr(FCall(Id("~" ^ n), List.mapi (fun i _ -> Id("__p" ^ 
            (string_of_int i))) ft.param_typs))
      ] else [
        VDecl(ft.return_typ, "__ret", Some(FCall(Id("~" ^ n), 
            List.mapi (fun i _ -> Id("__p" ^ (string_of_int i))) ft.param_typs)));
        Return(Id("__ret"))
      ];}))
    | _ -> (t, SId n))

  | Assign(e1, e2) ->
    let (t1, se1) = match e1 with
        Id(n) -> let t = find_in_ctxt n ctxt in
        (t, SId n)
      | _ -> check_expr ctxt e1 
    in
    let (t2, se2) = check_expr ctxt e2 in
    (check_asn t1 t2, SAssign((t1, se1), (t2, se2)))

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
                        ("struct " ^ s.sstruct_name ^ 
                            " has no member " ^ field_name)))
          | _ -> print_endline(fmt_styp struct_type); 
            raise (Failure "dot operator used on non-struct type"))
      in (access_type, st)
    in
    let (t1, se1) = check_expr ctxt e in
    let (field_type, _ ) = check_struct_access t1 field_name 
    in (field_type, SDot((t1, se1), field_name))

  | Binop(e1, op, e2) ->
    let (lt, se1) = check_expr ctxt e1 in
    let (rt, se2) = check_expr ctxt e2 in
    let sbinop = SBinop((lt, se1), op, (rt, se2)) in
    (match op with
       Add | Sub | Mult | Div | Mod when lt = SInt && rt = SInt 
       -> (SInt, sbinop)
     | Add | Sub | Mult | Div when lt = SFloat && rt = SFloat 
       -> (SFloat, sbinop)
     | Add when lt = SString && rt = SString -> (SString,sbinop)
     | Equal | Neq  when lt = rt -> (SBool, sbinop)
     | Less | Leq | Greater | Geq  
       when (lt = SInt && rt = SInt) 
         || (lt = SFloat || rt = SFloat) -> 
       (SBool, sbinop)
     | And | Or when lt = SBool && rt = SBool -> (SBool, sbinop)
     | _ -> raise (Failure("Error: cannot use " ^ fmt_op op ^ 
                           " with types: "^ fmt_styp rt ^ " and " ^ fmt_styp lt )))

  | Unop(op, e) -> 
    let (t, e) = check_expr ctxt e in 
    let sunop = SUnop(op, (t, e)) in
    (match op with 
       Neg when t = SInt -> (SInt, sunop)
     | Neg when t = SFloat -> (SFloat, sunop)
     | Not when t = SBool -> (SBool, sunop)
     | _ -> raise (Type_mismatch "Type mismatch for unary operator"))

  | FCall(expr, args) ->
    let check_args f_type args =
      let rec helper sl = function
          ([], []) -> sl
        | (p_typ::pl, arg::al) ->
          let (a_typ, se) = check_expr ctxt arg in
          if compare_typs p_typ (ignore_structs a_typ) 
            then helper ((a_typ, se)::sl) (pl, al)
          else raise (Failure "argument type mismatch")
        | _ -> raise (Failure "invalid number of arguments")
      in
      helper [] (f_type.sparam_typs, args)
    in
    let (t, se) = match expr with
      Id(s) when not (String.contains s '~') -> (find_in_ctxt s ctxt, SId(s))
    | _ -> check_expr ctxt expr 
    in
    let (func_t, sl) = match t with
        SFunc(func_t) -> (func_t, check_args func_t args)
      | _ -> raise (Failure "not a function")
    in
    (func_t.sreturn_typ, SFCall((t, se), sl))

  | FExpr(fexpr) ->
    let conv_params (typ, _ ) = (ignore_structs (styp_of_typ ctxt typ)) in
    let conv_params_with_both_fields (typ, str) = 
      (ignore_structs (styp_of_typ ctxt typ),str) in
    let sfunc_t = SFunc({
      sreturn_typ = ignore_structs (styp_of_typ ctxt fexpr.typ);
      sparam_typs = List.map conv_params fexpr.params;
      sbuiltin = false;
    }) in
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
    ignore (check_asn (ignore_structs return_t) 
      (ignore_structs (styp_of_typ ctxt fexpr.typ)));
    (sfunc_t, SFExpr({
         sname = fexpr.name;
         styp = ignore_structs(styp_of_typ ctxt fexpr.typ);
         sparams = List.map conv_params_with_both_fields fexpr.params;
         sbody = sl;
         srecursive = false;
       }))

  | Noexpr -> (SVoid, SNoexpr)
  | _ as x -> print_endline(Ast.fmt_expr x); 
    raise (Failure "not implemented in semant")

and styp_of_typ ctxt = function
    Int -> SInt
  | Bool -> SBool
  | Float -> SFloat
  | String -> SString
  | Void -> SVoid
  | Func f -> SFunc({ sparam_typs = List.map (styp_of_typ ctxt) f.param_typs; 
    sreturn_typ = styp_of_typ ctxt f.return_typ; sbuiltin = false; })
  | Struct s -> find_in_ctxt s.struct_name ctxt
  | Array t -> SArray(styp_of_typ ctxt t) 
  | ABSTRACT -> SABSTRACT

and check_stmt_list (ctxt : styp StringMap.t list) = function
    [] -> (ctxt, SVoid, [])
  | hd::tl -> 
    let (nctxt, t, ss) = check_stmt ctxt hd in
    let (nctxt, t_rest, ssl) = check_stmt_list nctxt tl in
    let ret =
      if t = SVoid
      then t_rest 
      else (if List.length tl <> 0 then raise 
        (Failure "dead code after return") else (); t)
    in
    (nctxt, ret, ss::ssl) (* returned something *)

and check_bool_expr (ctxt : styp StringMap.t list) e = 
  let (t, st) = check_expr ctxt e in
  if (t <> SBool) then 
    raise (Failure("Error: " ^ fmt_styp t ^ " is not a boolean type"))
  else (t, st)

(* returns the map, type, stype *)
and check_stmt (ctxt : styp StringMap.t list) = function
    Expr(e) -> let (t, ss) = 
                 check_expr ctxt e in (ctxt, SVoid, SExpr((t, ss)))
  | Destruct(keys, src, inner) ->
    let (t, se) = check_expr ctxt src in
    let tmp_n = "__tmp" ^ inner in
    let nctxt = add_to_ctxt t tmp_n ctxt in
    let init = SVDecl(t, tmp_n, Some((t, se))) in
    let members = match t with 
        SStruct(struct_t) -> struct_t.smembers
      | _ -> raise (Failure "cannot destruct non-struct")
    in
    let helper = function
        Expr(Id(x)) -> 
        let (ty, _) = StringMap.find x members in
        let v = check_expr nctxt (Dot(Id(tmp_n), x)) in
        (ty, x, SVDecl(ty, x, Some(v)))
      | _ -> raise (Failure "should've been caught in parser")
    in
    let (nctxt, decls) = List.fold_left 
      (fun (ctxt, decls) k -> 
          let (t, n, decl) = helper k in 
          (add_to_ctxt t n ctxt, decl::decls)) 
      (nctxt, [init]) keys in
    (nctxt, SVoid, SVBlock(List.rev decls))
  | VDecl(ltype, n, i) ->
    let t = match ltype with 
        Struct(struct_t) -> find_in_ctxt struct_t.struct_name ctxt 
      | _ -> styp_of_typ ctxt ltype
    in
    let t = ignore_structs t in
    (match i with
       None -> (add_to_ctxt t n ctxt, SVoid, SVDecl(t, n, None))
     | Some(e) ->
       let (t_i, s_i) = check_expr ctxt e in
       let t_i = ignore_structs t_i in
       let nctxt = add_to_ctxt t n ctxt in
       (nctxt, SVoid, SVDecl((check_asn t_i t), n, Some((t_i, s_i)))))

  | StructDef(name, fields) ->
    let conv_typ typ = ignore_structs (styp_of_typ ctxt typ) in
    let helper (map, list) (lt, n, i) =
      let lt = conv_typ lt in
      match lt with
        SStruct(struct_t) when name = struct_t.sstruct_name -> 
            raise (Failure ("illegal recursive struct " ^ name))
      | _ ->
        try
          match (StringMap.find n map) with
            _ -> raise (Failure (n ^ " already declared in struct " ^ name))
        with Not_found ->
          let (init, opt_se) = match i with
              None -> (None, None)
            | Some e ->
              let (rt, se) = check_expr ctxt e in
              let _ = check_asn (lt) rt in
              (Some(rt, se), Some(rt, se))
          in
          let new_map = StringMap.add n (lt, init) map in
          let new_list = (lt, n, opt_se)::list in
          (new_map, new_list)
    in
    let (members, fields_se) = List.fold_left helper 
      (StringMap.empty, []) fields in
    let struct_t = SStruct({
        sstruct_name = name;
        smembers = members;
        sincomplete = false;
        signore = false;
      }) in
    let nctxt = add_to_ctxt struct_t name ctxt in
    (nctxt, SVoid, SStructDef(name, fields_se))

  | Return(e) -> 
    let (t, ss) = check_expr ctxt e in 
    (ctxt, t, SReturn((t, ss)))
  | ForLoop (s1, e2, e3, st) -> 
    let (ctxt1, s1') = match s1 with
        None -> (StringMap.empty::ctxt, None)
      | Some(s1) -> (let (nctxt, _, ns1) = check_stmt (StringMap.empty::ctxt) s1 in
                     (nctxt, Some(ns1)))
    in
    let (ctxt2, e2') = match e2 with
        None -> (ctxt1, None)
      | Some(e2) -> (let (t_i, si) = 
                       check_bool_expr ctxt1 e2 in (ctxt1, Some((t_i, si))))
    in
    let (ctxt3, e3') = match e3 with
        None -> (ctxt2, None)
      | Some(e3) -> (let (t_i, si) = 
                       check_expr ctxt2 e3 in (ctxt2, Some((t_i, si))))
    in
    let (_, ret_t, st') = check_stmt_list ctxt3 st
    in
    (ctxt, ret_t, SForLoop(s1', e2', e3', st'))

  | If (e, st1, st2) ->
    let e' = check_bool_expr ctxt e
    in
    let ifctxt = StringMap.empty::ctxt in
    let (_, rt1, st1') = check_stmt_list ifctxt st1
    in
    let (_, rt2, st2') = check_stmt_list ifctxt st2
    in
    let rt = match rt1, rt2 with
        (SVoid, _) -> SVoid
      | (_, SVoid) -> SVoid
      | (t1, t2) -> check_asn t1 t2
    in
    (ctxt, rt, SIf(e', st1', st2'))

  | _ -> (ctxt, SVoid, SExpr((SVoid, SNoexpr)))

let builtins = [
  ("println", SFunc({ sparam_typs = [SString]; sreturn_typ = SVoid; 
    sbuiltin = true; }));
  ("print", SFunc({ sparam_typs = [SString]; sreturn_typ = SVoid; 
    sbuiltin = true; }));
  ("str_of_int", SFunc({ sparam_typs = [SInt]; sreturn_typ = SString; 
    sbuiltin = true; }));
  ("str_of_bool", SFunc({ sparam_typs = [SBool]; sreturn_typ = SString; 
    sbuiltin = true; }));
  ("string_concat", SFunc({ sparam_typs = [SString; SString]; 
    sreturn_typ = SString; sbuiltin = true; })); 
  ("string_equals", SFunc({ sparam_typs = [SString; SString]; 
    sreturn_typ = SInt; sbuiltin = true; })); 
  ("str_of_float", SFunc({ sparam_typs = [SFloat]; 
    sreturn_typ = SString; sbuiltin = true; })); 
  ("int_of_float", SFunc({ sparam_typs = [SFloat]; 
    sreturn_typ = SInt; sbuiltin = true; })); 
  ("float_of_int", SFunc({ sparam_typs = [SInt]; 
    sreturn_typ = SFloat; sbuiltin = true; })); 
  ("scan_line", SFunc({ sparam_typs = [SInt]; 
    sreturn_typ = SString; sbuiltin = true; })); 
  ("exit_success", SFunc({ sparam_typs = []; 
    sreturn_typ = SVoid; sbuiltin = true; })); 
  ("die", SFunc({ sparam_typs = [SString; SInt]; 
    sreturn_typ = SVoid; sbuiltin = true; })); 
  ("int_of_str", SFunc({ sparam_typs = [SString]; 
    sreturn_typ = SInt; sbuiltin = true; })); 
  ("rand_autoseed", SFunc({ sparam_typs = []; 
    sreturn_typ = SVoid; sbuiltin = true; }));
  ("rand_afterseed", SFunc({ sparam_typs = []; 
    sreturn_typ = SInt; sbuiltin = true; }));  
  ("scan_char", SFunc({ sparam_typs = []; sreturn_typ = SString; 
    sbuiltin = true; }));  
]

let def_ctxt =
  let add_func ctxt (name, func_t) = add_to_ctxt func_t name ctxt in
  List.fold_left add_func [StringMap.empty] builtins

let check_program (prog : stmt list) =
  if List.exists (fun x -> match x with Return(_) -> true | _ -> false) prog
  then raise (Failure "illegal return statement")
  else
    let (_, _, ssl) = check_stmt_list def_ctxt prog in
    List.flatten (List.map 
      (fun s -> match s with SVBlock(sl) -> sl | _ as x -> [x]) ssl)

