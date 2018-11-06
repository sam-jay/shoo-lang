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
  
let check_asn lvalue_t rvalue_t =
  let found_match = match lvalue_t, rvalue_t with
    Struct(s_l), Struct(s_r) -> s_l.struct_name = s_r.struct_name
  | _ -> lvalue_t = rvalue_t in
  if found_match
  then lvalue_t
  else raise (Type_mismatch "type mismatch error")

(* This function takes a tuple with the type and the map 
 * as well as the variable name and the context map.
 * The map in the tuple is used for the member fields
 * in structs. The map is None unless you are adding a new
 * struct type. *)    
let add_to_ctxt (v_type : typ) (v_name : string) (ctxt : typ StringMap.t list) =
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
let rec find_in_ctxt (v_name : string) (ctxt : typ StringMap.t list) =
  try
    StringMap.find v_name (List.hd ctxt)
  with Not_found -> match List.tl ctxt with
    [] -> raise (Undeclared_reference ("undeclared reference " ^ v_name))
  | tail -> find_in_ctxt v_name tail

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
let rec check_expr (ctxt : typ StringMap.t list) = function
| IntLit(x) -> (ctxt, (Int, SIntLit x))
| BoolLit(x) -> (ctxt, (Bool, SBoolLit x))
| FloatLit(x) -> (ctxt, (Float, SFloatLit x))
| StrLit(x) -> (ctxt, (String, SStrLit x))

| New(NStruct(name)) ->
    let t = find_in_ctxt name ctxt in
    (ctxt, (t, SNew(SNStruct(name))))
    
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
    let (_, (item_type, item_s_type)) = check_expr ctxt (List.hd x) in
    let t = List.map (fun e1 ->
        let (_, (t1, st1)) = check_expr ctxt e1 in
        (* TODO(claire) need to check both? *)
        if t1 = item_type && st1 = item_s_type then (t1, st1)
        else raise (Failure ("Multiple types inside an array"))
        (* TODO(claire) add pretty print for error above *)
    ) x in (ctxt, (item_type, SArrayLit t))

| ArrayAccess(arr_name, int_expr) ->
    let (nctxt, (t1, se1)) = check_expr ctxt int_expr 
    in
    if t1 = Int then (nctxt, (t1, SArrayAccess(arr_name, (t1, se1))))
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
      (* TODO(claire) need to pretty print errors below *)
      let (access_type, _) = (match struct_type with
        (* make sure you were passed a struct *)
        Struct(s) ->
          (let t = find_in_ctxt s.struct_name ctxt in (* Get the complete struct with members *)
          let struct_t = match t with Struct(st) -> st | _ -> raise (Failure "shouldn't happen") in
          try StringMap.find field_name struct_t.members
          with Not_found -> raise (Failure ("struct " ^ s.struct_name ^ " has no member " ^ field_name)))
      | _ -> raise (Failure "dot operator used on non-struct type"))
      in access_type
    in
    let (_, (t1, se1)) = check_expr ctxt e in
    let field_type = check_struct_access t1 field_name 
    in (ctxt, (field_type, SDot((field_type, se1), field_name)))

| Binop(e1, op, e2) ->
    let (nctxt, (lt, se1)) = check_expr ctxt e1 in
    let (nctxt, (rt, se2)) = check_expr nctxt e2 in
    let sbinop = SBinop((lt, se1), op, (rt, se2)) in
    (match op with
      Add | Sub | Mult | Div when lt = Int && rt = Int -> (nctxt, (Int, sbinop))
    | Add | Sub | Mult | Div when lt = Float && rt = Float -> (nctxt, (Float, sbinop))
    (* Allow for ints and floats to be used together. *)
    | Add | Sub | Mult | Div when 
        (lt = Float && rt = Int) ||
        (lt = Int && rt = Float) -> (nctxt, (Float, sbinop))
    (* TODO(claire): make sure LRM says that we can compare all
      * expressions of the same type using ==, including functions, 
      * strings,
      * structs, arrays? *)
    | Equal | Neq  when lt = rt -> (nctxt, (Bool, sbinop))
    | Equal | Neq  when 
        (lt = Float && rt = Int) ||
        (lt = Int && rt = Float) -> (nctxt, (Bool, sbinop))
    | Equal | Neq  when lt = Bool && rt = Bool -> 
            (nctxt, (Bool, sbinop))
    | Less | Leq | Greater | Geq  
                              when (lt = Int && rt = Int) 
                              || (lt = Float || rt = Float) -> 
                                      (nctxt, (Bool, sbinop))
    | And | Or when rt = Bool && rt = Bool -> (nctxt, (Bool, sbinop))
    | _ -> raise (Type_mismatch "Type mismatch across binary operator"))
    (* TODO(claire) need to pretty print error above *)

| Unop(op, e) -> 
    let (nctxt, (t, e)) = check_expr ctxt e in 
    let sunop = SUnop(op, (t, e)) in
    (match op with 
      Neg when t = Int -> (nctxt, (Int, sunop))
    | Neg when t = Float -> (nctxt, (Float, sunop))
    | Not when t = Bool -> (nctxt, (Bool, sunop))
    | _ -> raise (Type_mismatch "Type mismatch for unary operator"))

| Pop(e, op) ->
    let (nctxt, (t, e)) = check_expr ctxt e in 
    let spop = SPop((t, e), op) in
    (match op with 
      Inc when t = Int -> (nctxt, (Int, spop))
    | Dec when t = Int -> (nctxt, (Int, spop))
    | _ -> raise (Type_mismatch "Type mismatch for unary operator"))

| FCall(expr, args) ->
    let check_args f_type args =
      let rec helper sl = function
        ([], []) -> sl
      | (p_typ::pl, arg::al) ->
        let (_, (a_typ, se)) = check_expr ctxt arg in
        if p_typ = a_typ then helper ((a_typ, se)::sl) (pl, al)
        else raise (Failure "argument type mismatch")
      | _ -> raise (Failure "invalid number of arguments")
      in
      helper [] (f_type.param_typs, args)
    in
    let (_, (t, se)) = check_expr ctxt expr in
    let (func_t, sl) = match t with
      Func(func_t) -> (func_t, check_args func_t args)
    | _ -> raise (Failure "not a function")
    in
    (ctxt, (func_t.return_typ, SFCall((t, se), sl)))

| FExpr(fexpr) ->
    let func_t = Func({ return_typ = fexpr.typ; param_typs = List.map fst fexpr.params }) in
    let create_scope list =
      let rec helper m = function
        [] -> m
      | (t, n)::tl -> 
          let new_m = StringMap.add n t m in 
              helper new_m tl
      in
      if fexpr.name != ""
      then helper (StringMap.add fexpr.name func_t StringMap.empty) list
      else helper StringMap.empty list
    in
    let func_scope = create_scope fexpr.params in
    let (_, return_t, sl) = check_stmt_list (func_scope::ctxt) fexpr.body in
    check_asn return_t fexpr.typ;
    (ctxt, (func_t, SFExpr({
      sname = fexpr.name;
      styp = fexpr.typ;
      sparams = fexpr.params;
      sbody = sl;
      srecursive = false; (* TODO: handle recursion *)
    })))

| Noexpr -> (ctxt, (Void, SNoexpr))
| _ as x -> print_endline(Ast.fmt_expr x); raise (Failure "not implemented in semant")

and check_stmt_list (ctxt : typ StringMap.t list) = function
  [] -> (ctxt, Void, [])
| hd::tl -> 
  let (nctxt, t, ss) = check_stmt ctxt hd in
  let (nctxt, t_rest, ssl) = check_stmt_list nctxt tl in
  let ret =
    if t = Void
    then t_rest 
    else (if List.length tl != 0 then raise (Failure "dead code after return") else (); t)
  in
  (nctxt, ret, ss::ssl) (* returned something *)

and check_bool_expr (ctxt : typ StringMap.t list) e = 
    let (nctxt, (t, st)) = check_expr ctxt e in
    if (t != Bool) then raise (Failure "expected Boolean expression")
    (* TODO(claire) add pretty print above *) 
    else (nctxt, (t, st)) 

(* returns the map, type, stype *)
and check_stmt (ctxt : typ StringMap.t list) = function
  Expr(e) -> let (nctxt, (t, ss)) = 
      check_expr ctxt e in (nctxt, Void, SExpr((t, ss)))
| VDecl(ltype, n, i) ->
  let t = match ltype with 
    Struct(struct_t) -> find_in_ctxt struct_t.struct_name ctxt 
    | _ -> ltype
  in
  (match i with
    None -> (add_to_ctxt t n ctxt, Void, SVDecl(t, n, None))
  | Some(e) ->
      let (_, (t_i, s_i)) = match e with
        StructInit(assigns) -> 
          let struct_t = match t with Struct(st) -> st | _ -> raise (Type_mismatch "type mismatch error") in
          let check_init (name, exp) =
            let (_, (t, se)) = check_expr ctxt exp in
            try
              let (mt, _) = StringMap.find name struct_t.members in
              (name, (check_asn t mt, se))
            with Not_found -> raise (Failure ("struct " ^ struct_t.struct_name ^ " has no member " ^ name))
          in
          let slist = List.map check_init assigns in
          (ctxt, (Struct(struct_t), SStructInit(slist)))
      | _ -> check_expr ctxt e
      in
      let nctxt = add_to_ctxt t n ctxt in
      (nctxt, Void, SVDecl((check_asn t_i t), n, Some((t_i, s_i)))))

| StructDef(name, fields) ->
    let helper (map, list) (lt, n, i) =
      match lt with
        Struct(struct_t) when name = struct_t.struct_name -> raise (Failure ("illegal recursive struct " ^ name))
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
          (Some(e), Some(rt, se))
        in
        let new_map = StringMap.add n (lt, init) map in
        let new_list = (lt, n, opt_se)::list in
        (new_map, new_list)
    in
    let (members, fields_se) = List.fold_left helper (StringMap.empty, []) fields in
    let struct_t = Struct({
      struct_name = name;
      members = members;
      incomplete = false;
    }) in
    let nctxt = add_to_ctxt struct_t name ctxt in
    (nctxt, Void, SStructDef(name, fields_se))

(*
| StructDef(name, fields) ->
    (* Create a map of the member fields. 
     * See if there are repeat variables. *)
    let vdecl_repeats_map = List.fold_left (fun map v_field ->
        let get_name (_,n,_) = n in
        let v_name = get_name v_field in
        (* If the variable is in the map, it is a repeat and will have 
         * a type. *)
        (let ((t_opt,_), _) = find_in_ctxt v_name map in
        match t_opt with
            Some(_) -> 
                raise (Failure "can't repeat variable names in structs")
            | None ->
                 (let get_init (_,_,i) = i in
                 let v_init = get_init v_field in
                 let get_type (t,_,_) = t in
                 let field_type = get_type v_field in
                 (* Check the type to ensure there isn't a recusrive
                  * definition. *)
                 let v_type = 
                     (* See if there is a recursive struct def by
                      * comparing the variable type and the struct type *)
                     if field_type = Struct(name) then
                        raise (Failure "can't have recursive struct def")
                     else field_type in 
                 let add_map = match v_init with
                    None ->
                        (* The map is None because you can't define a
                         * struct inside of a struct. *)
                        (add_to_ctxt (v_type, None) v_name map)
                    | Some(e) -> let (_, (t_i, _)) =
                        (* check_expr doesn't change the map *)
                        check_expr map e 
                        in           
                        let matching_type = 
                            (* see if the expression matches 
                             * the given type *)
                            (* TODO(claire) I am not sure if check_assign 
                             * deals with struct and array comparision...*)
                            check_assign v_type t_i
                            (Failure ("illegal assignment in struct"))
                            (* TODO(claire) pretty print error above *)
                        in
                        add_to_ctxt (matching_type, None) v_name map 
                  in add_map))
           ) (* end of function *) [StringMap.empty] fields
    in
    (* make a list of all the types in the struct *)
    let field_types = List.map (fun v_field -> 
        let get_name (_,n,_) = n in
        let v_name = get_name v_field in
        let get_init (_,_,i) = i in
        let v_init = get_init v_field in
        (* If there is an expression, get the type of the expression.
         * Above should have handled the case where the given type and
         * the expression types don't match. *)
        let find_expression_type = (match v_init with
            None -> None
            | Some(e) ->
             (let (_, (t_i, si)) =
             check_expr vdecl_repeats_map e in
             Some((t_i, si))))
        in 
        let get_type (t,_,_) = t in
        let v_type = get_type v_field in
        (v_type, v_name, find_expression_type)
        ) (* end of function for List.map *) fields 
    in 
    (* name is the name of the struct type *)
    (* Add the name of the struct type, its type, and the map of its
     * members to the ctxt. *)
    (add_to_ctxt (Struct(name), 
        Some(List.hd vdecl_repeats_map)) name ctxt, Void,
        SStructDef(name, field_types))
*)


| Return(e) -> let (nctxt, (t, ss)) = 
    check_expr ctxt e in (nctxt, t, SReturn((t, ss)))
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
        None -> (ctxt, None)
        | Some(e3) -> (let (nctxt, (t_i, si)) = 
            check_expr ctxt2 e3 in (nctxt, Some((t_i, si))))
     in
     let (ctxt4, _, st') = check_stmt_list ctxt3 st
     in
    (ctxt4, Void, SForLoop(s1', e2', e3', st'))

(* Note: Handling the context variable of two branches is kinda tricky because
   it does not follow a linear flow. My assumption is that everything 
   defined in the block should not be effective outside of the if block
   and that it should be consistent between For and If. *)
| If (e, st1, st2) ->
     let (ctxt1, e') = check_bool_expr ctxt e
     in
     let (_, _, st1') = check_stmt_list ctxt1 st1
     in
     let (_, _, st2') = check_stmt_list ctxt1 st2
     in
    (ctxt, Void, SIf(e', st1', st2'))
    
| _ -> (ctxt, Void, SExpr((Void, SNoexpr)))

let def_ctxt =
  let println_t = Func({
    param_typs = [String];
    return_typ = Void
  }) in
  let ctxt = add_to_ctxt println_t "println" [StringMap.empty] in
  ctxt

let check_program (prog : stmt list) =
  let (_, _, ssl) = check_stmt_list 
  def_ctxt 
  prog 
  in
  ssl

