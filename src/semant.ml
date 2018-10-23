open Ast

module StringMap = Map.Make (String)

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

let find_in_ctxt v_name ctxt =
  let rec helper init = function
    [] -> (None, init)
  | hd::tl when StringMap.mem v_name hd ->
      (Some(StringMap.find v_name hd), init)
  | _::tl -> helper false tl in
  helper true ctxt

let rec check_expr ctxt = function
| IntLit(x) -> (ctxt, Int)
| FloatLit(x) -> (ctxt, Float)
| Id(n) -> 
    let (t_opt, local) = find_in_ctxt n ctxt in
    (match t_opt with
      Some((t, i)) ->
      (match i with
         true -> (ctxt, t)
       | false -> raise (Failure "uninitialized variable"))
    | None -> raise (Failure "undeclared reference"))
| Assign(e1, e2) ->
    let (nctxt, t2) = check_expr ctxt e2 in
    let (nctxt, t1) = match e1 with
        Id(n) -> let (t_opt, local) = find_in_ctxt n nctxt in
                (match t_opt with
                  Some(t, _) -> (nctxt, t)
                | None -> raise (Failure "undeclared reference"))
      | _ -> check_expr nctxt e1 in
    if t1 = t2 then (nctxt, t1)
    else raise (Failure "type mismatch in assignment")
| _ -> (ctxt, Void)

let rec check_stmt ctxt = function
  Expr(e) -> check_expr ctxt e
| VDecl(t, n, i) ->
  let (t_opt, local) = find_in_ctxt n ctxt in
  (match t_opt with
    None -> (add_to_ctxt t n i ctxt, Void) 
  | Some(_) when not local -> (add_to_ctxt t n i ctxt, Void)
  | Some(_) -> raise (Failure "already declared"))
| _ -> (ctxt, Void)

let rec check_stmt_list ctxt = function
  [] -> ctxt
| hd::tl -> 
  let (new_ctxt, t) = check_stmt ctxt hd in
  check_stmt_list new_ctxt tl

let foo = (Some(Int), true)
let check_program prog = check_stmt_list [StringMap.empty] prog

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  check_program program
