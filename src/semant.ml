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

(* This function takes a tuple with the type and the map 
 * as well as the variable name and the context map.
 * The map in the tuple is used for the member fields
 * in structs. *)    
let add_to_ctxt (v_type, v_member_map) v_name ctxt =
  let map = List.hd ctxt in
  (* TODO(claire): why rename v_type to v? *)
  let v = v_type in
  let newMap = StringMap.add v_name (v, v_member_map) map in
  newMap::List.tl ctxt

(* Returns a tuple with the type and the map and if
 * the variable is initalized or not. The type and
 * map are optional. *)
let find_in_ctxt v_name ctxt =
  let rec helper init = function
    [] -> ((None, None), init)
  | hd::_ when StringMap.mem v_name hd ->
     (* TODO(claire) need to change this to give the map
      * only if the type is struct *) 
    (* See if there is a map, meaning that you have a 
     * struct type. *)
    let (v_type, v_map) = StringMap.find v_name hd in
    (match v_map with
        None -> ((Some(v_type), None), init)
        | Some(m) -> ((Some(v_type), Some(m)), init)) 
  | _::tl -> helper false tl in
  helper true ctxt

let create_scope list = 
 let rec helper m = function
   [] -> m
 (* TODO(claire) need to change this so it added the map
  * if the type is struct. The map will be in the ctxt
  * because the struct will already have to be defined.
  * This function now has to take the ctxt in which the
  * function is being declared. I am not sure how the
  * nested scopes work, but it should also allow you do use
  * struct types declared in the outer scopes. *)
 | ((t, members), n)::tl -> 
         (*(match members with
         Some(members) ->
            let new_m = StringMap.add n (t, members) m 
                 in helper new_m tl
         | None ->
            let new_m = StringMap.add n (t, None) m 
                 in helper new_m tl)*)
            let new_m = StringMap.add n (t, members) m 
                 in helper new_m tl
 in helper StringMap.empty list

let get_members_if_struct v_type ctxt =
        try let get_struct_name (Struct(n)) = n in
            let struct_name = get_struct_name v_type in
            let ((t,m), _) = find_in_ctxt struct_name ctxt
            in (v_type,m)
        with Not_found -> 
            (* Not a struct so shouldn't have a members map *)
            (v_type, None)
        (*if v_type = Struct(_) then 
            let get_struct_name Struct(n) = n in
            let struct_name = get_struct_name v_type in
            (* The struct name is the name stored in the ctxt *) 
            let ((t, m), _) = find_in_ctxt struct_name ctxt
        in (v_type, m)*)

(* Returns a tuple with a map and another tuple.
 * The second tuple has the type and the stype. *)
let rec check_expr ctxt = function
| IntLit(x) -> (ctxt, (Int, SIntLit x))
| BoolLit(x) -> (ctxt, (Bool, SBoolLit x))
| FloatLit(x) -> (ctxt, (Float, SFloatLit x))
| StrLit(x) -> (ctxt, (String, SStrLit x))
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
    let ((t_opt, _), _) = find_in_ctxt n ctxt in
    (match t_opt with
      Some(t) -> (ctxt, (t, SId n))
    | None -> raise (Undeclared_reference "undeclared reference"))
| Assign(e1, e2) ->
    let (nctxt, (t2, se2)) = check_expr ctxt e2 in
    let (nctxt, (t1, se1)) = match e1 with
        Id(n) -> let ((t_opt,_), _) = find_in_ctxt n nctxt in
                (match t_opt with
                  Some(t) -> (nctxt, (t, SId n))
                | None -> raise (Undeclared_reference "undeclared reference"))
      | _ -> check_expr nctxt e1 in
    if t1 = t2 then (nctxt, (t1, SAssign((t1, se1), (t2, se2))))
    else raise (Type_mismatch "type mismatch in assignment")
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
         * expressions of the same type using ==, including functions, strings,
         * structs, arrays? *)
        | Equal | Neq  when lt = rt -> (nctxt, (Bool, sbinop))
        | Equal | Neq  when 
            (lt = Float && rt = Int) ||
            (lt = Int && rt = Float) -> (nctxt, (Bool, sbinop))
        | Equal | Neq  when lt = Bool && rt = Bool -> (nctxt, (Bool, sbinop))
        | Less | Leq | Greater | Geq  
                                 when (lt = Int && rt = Int) 
                                 || (lt = Float || rt = Float) -> (nctxt, (Bool, sbinop))
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
| FCall(name, args) ->
  let ((t_opt, _), _) = find_in_ctxt name ctxt in
  (match t_opt with
    Some(t) -> 
      let (nctxt, sl) = check_args ctxt t args in
      (nctxt, (t, SFCall(name, sl)))
  | _ -> raise (Undeclared_reference ("undeclared function " ^ name)))
| _ -> (ctxt, (Void, SNoexpr))

(* Make sure that types of arguements match the types of
 * formal parameters when you declare a func variable. *)
and check_args ctxt t args =
  match t with Func(f_type) ->
  let rec helper ctxt sl = function
    ([], []) -> (ctxt, sl)
  | (p_typ::pl, arg::al) ->
    let (nctxt, (a_typ, se)) = check_expr ctxt arg in
    if p_typ = a_typ then helper nctxt ((a_typ, se)::sl) (pl, al)
    else raise (Failure "argument type mismatch")
  | _ -> raise (Failure "invalid number of arguments")
  in
  helper ctxt [] (f_type.param_typs, args)
  | _ -> raise (Failure "unknown")

let rec check_stmt_list ctxt = function
  [] -> (ctxt, Void, [])
| hd::tl -> 
  let (nctxt, t, ss) = check_stmt ctxt hd in
  let (nctxt, t_rest, ssl) = check_stmt_list nctxt tl in
  let ret = if t = Void then t_rest else t in
  (nctxt, ret, ss::ssl) (* returned something *)

and check_bool_expr ctxt e = 
    let (nctxt, (t, st)) = check_expr ctxt e in
    if (t != Bool) then raise (Failure "expected Boolean expression")
    (* TODO(claire) add pretty print above *) 
    else (nctxt, (t, st)) 

(* returns the map, type, stype *)
and check_stmt ctxt = function
  Expr(e) -> let (nctxt, (t, ss)) = 
      check_expr ctxt e in (nctxt, Void, SExpr((t, ss)))
| VDecl(t, n, i) ->
  let (nctxt, si) = match i with
    None -> (ctxt, None)
  | Some(e) -> (let (nctxt, (t_i, si)) = 
      check_expr ctxt e in (nctxt, Some((t_i, si)))) in
  let ((t_opt, _), local) = find_in_ctxt n nctxt in
  (match t_opt with
    None -> (add_to_ctxt (t, None) n nctxt, Void, SVDecl(t, n, si)) 
   (* TODO(claire): so we can have local vars with the 
     * same name as global vars and the local var wins over the 
     * global one? need to update LRM with this info abt scoping *)
  | Some(_) when not local -> (add_to_ctxt (t, None) n nctxt, 
            Void, SVDecl(t, n, si))
  | Some(_) -> raise (Failure "already declared"))
| StructDef(name, fields) ->
   (* See if there are repeat variables. *)
   (* TODO(claire): need to add this map to the ctxt as another optional
    * field to track the variables in a struct. *)
    (* TODO(claire): need to figure out how to detect mutually 
     * recursive struct defs *)
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
                 (* add the expression to the map *)
                 let add_map = match v_init with
                    None -> (add_to_ctxt (v_type, None) v_name map)
                    | Some(e) -> let (_, (t_i, _)) =
                        (* check_expr doesn't change the map *)
                        check_expr map e 
                        in           
                        let matching_type = 
                            (* see if the expression matches 
                             * the given type *)
                            check_assign v_type t_i
                            (Failure ("illegal assignment in struct"))
                        in
                            (* TODO(claire) pretty print error above *)
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
        ) (* end of function*) fields 
    in 
    (add_to_ctxt (Struct(name), None) name ctxt, Void,
                SStructDef(name, field_types))
| FDecl(name, params, ret, body) ->
  let f_type = Func({
    param_typs = List.map (fun (t, _) -> 
        (* TODO(claire) leave this as is for now and change
         * create_scope to call get_members_if_struct itself *)
        (* Get the member map from the ctxt if you have
         * a struct type *)
        (*let (v_type, v_map) =
            if v_type = Struct() *)
    (*    let (v_type, v_map) = get_members_if_struct t ctxt 
            in (v_type, v_map)*) t) params;
    return_typ = ret;
  }) in
  (* TODO(claire) why is this here? It gives unusd variable warnings
   * because of this. *)
  let init = Some(FExpr({
    typ = ret;
    params = params;
    body = []
  })) in
  let nctxt = add_to_ctxt (f_type, None) name ctxt in
  let nctxt = (create_scope params)::nctxt in
  let (nctxt, t, ssl) = check_stmt_list nctxt body in
  if t = ret then (List.tl nctxt, Void, SFDecl(name, params, ret, ssl))
  else raise (Failure "invalid function return type")
| Return(e) -> let (nctxt, (t, ss)) = 
    check_expr ctxt e in (nctxt, t, SReturn (t, ss))
| ForLoop (s1, e2, e3, st) -> 
     let (ctxt1, s1') = match s1 with
        None -> (ctxt, None)
        | Some(s1) -> (let (nctxt, _, ns1) = check_stmt ctxt s1 in
            (nctxt, Some(ns1)))
     in
     let (ctxt2, e2') = match e2 with
        None -> (ctxt1, None)
        | Some(e2) -> (let (nctxt, (t_i, si)) = 
            check_expr ctxt1 e2 in (nctxt, Some((t_i, si))))
     in
     let (ctxt3, e3') = match e3 with
        None -> (ctxt, None)
        | Some(e3) -> (let (nctxt, (t_i, si)) = 
            check_expr ctxt2 e3 in (nctxt, Some((t_i, si))))
     in
     let (ctxt4, _, st') = check_stmt_list ctxt3 st
     in
    (ctxt4, Void, SForLoop(s1', e2', e3', st'))
    
| _ -> (ctxt, Void, SExpr((Void, SNoexpr)))

let def_ctxt =
  let println_t = Func({
    param_typs = [String];
    return_typ = Void
  }) in
  let ctxt = add_to_ctxt (println_t, None) "println" [StringMap.empty] in
  ctxt

let check_program prog =
  (*print_endline(Printer.fmt_prog prog);*)
  let (_, _, ssl) = check_stmt_list def_ctxt prog in
  ssl
