open Ast
open Sast

module StringMap = Map.Make (String)

exception Invalid_assignment of string
exception Undeclared_reference of string
exception Illegal_binary_operator of string

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

let add_to_ctxt v_type v_name init ctxt =
  let map = List.hd ctxt in
  let initialized = match init with None -> false | Some(_) -> true in
  let v = (v_type, initialized) in
  let newMap = StringMap.add v_name v map in
  newMap::List.tl ctxt

(* Returns tuple with None or Some with another tuple that has 
 * the type and if initalized or not and then if it is 
 * definited in scope or not. *)
let find_in_ctxt v_name ctxt =
  let rec helper init = function
    [] -> (None, init)
  | hd::_ when StringMap.mem v_name hd ->
      (Some(StringMap.find v_name hd), init)
  | _::tl -> helper false tl in
  helper true ctxt

let create_scope list = 
 let rec helper m = function
   [] -> m
 | (t, n)::tl -> let new_m = StringMap.add n (t, true) m in helper new_m tl
 in helper StringMap.empty list

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
| Id(n) -> 
    let (t_opt, _) = find_in_ctxt n ctxt in
    (match t_opt with
      Some((t, i)) ->
      (match i with
         true -> (ctxt, (t, SId n))
       | false -> raise (Failure "uninitialized variable"))
    | None -> raise (Failure "undeclared reference"))
| Assign(e1, e2) ->
    let (nctxt, (t2, se2)) = check_expr ctxt e2 in
    let (nctxt, (t1, se1)) = match e1 with
        Id(n) -> let (t_opt, _) = find_in_ctxt n nctxt in
                (match t_opt with
                  Some(t, _) -> (nctxt, (t, SId n))
                | None -> raise (Failure "undeclared reference"))
      | _ -> check_expr nctxt e1 in
    if t1 = t2 then (nctxt, (t1, SAssign((t1, se1), (t2, se2))))
    else raise (Invalid_assignment "type mismatch in assignment")
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
        | _ -> raise (Illegal_binary_operator "illegal binary operator"))
        (* TODO(claire) need to pretty print error above *)
| Unop(op, e) -> 
        let (nctxt, (t, e)) = check_expr ctxt e in 
        let sunop = SUnop(op, (t, e)) in
        (match op with 
          Neg when t = Int -> (nctxt, (Int, sunop))
        | Neg when t = Float -> (nctxt, (Float, sunop))
        | Not when t = Bool -> (nctxt, (Bool, sunop))
        | _ -> raise (Failure("illegal unary operator")))
| FCall(name, args) ->
  (match find_in_ctxt name ctxt with
    (Some((t, true)), _) -> 
      let (nctxt, sl) = check_args ctxt t args in
      (nctxt, (t, SFCall(name, sl)))
  | _ -> raise (Failure ("unknown function")))
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
  Expr(e) -> let (nctxt, (t, ss)) = check_expr ctxt e in (nctxt, Void, SExpr((t, ss)))
| VDecl(t, n, i) ->
  let (nctxt, si) = match i with
    None -> (ctxt, None)
  | Some(e) -> (let (nctxt, (t_i, si)) = check_expr ctxt e in (nctxt, Some((t_i, si)))) in
  let (t_opt, local) = find_in_ctxt n nctxt in
  (match t_opt with
    None -> (add_to_ctxt t n i nctxt, Void, SVDecl(t, n, si)) 
  | Some(_) when not local -> (add_to_ctxt t n i nctxt, Void, SVDecl(t, n, si))
  | Some(_) -> raise (Failure "already declared"))
| FDecl(params, ret, body, r) ->
  let nctxt = (create_scope params)::ctxt in
  let (nctxt, t, ssl) = check_stmt_list nctxt body in
  if t = ret then (List.tl nctxt, Void, SFDecl(params, ret, ssl, r))
  else raise (Failure "invalid function return type")
| Return(e) -> let (nctxt, (t, ss)) = check_expr ctxt e in (nctxt, t, SReturn (t, ss))
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
  let init = Some(FExpr({
    typ = Void;
    params = [(String, "x")];
    body = []
  })) in
  let ctxt = add_to_ctxt println_t "println" init [StringMap.empty] in
  ctxt

let check_program prog =
  (*print_endline(Printer.fmt_prog prog);*)
  let (_, _, ssl) = check_stmt_list def_ctxt prog in
  ssl
