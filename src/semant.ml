open Ast

module StringMap = Map.Make (String)

(* READ-THIS!!

  ctxt is a list of StringMaps [ StringMap; StringMap; ... ]
  each StringMap is a map from string:v_name to (type, current_val)
  current_val is None or Some(v)

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

let find_in_ctxt v_name ctxt =

let add_to_ctxt v_type v_name init ctxt = 

let rec check_expr ctxt = function

let rec check_stmt ctxt = function
  Expr(e) -> check_expr ctxt e
| VDecl(t, n, i) -> 
  let x = find_in_ctxt n ctxt in
  (match x with
    None -> add_to_ctxt t n i ctxt
  | Some(_) -> raise Failure "already declared")
| _ -> ctxt

let rec check_stmt_list ctxt = function
  [] -> ctxt
| hd::tl -> 
  let new_ctxt = check_stmt ctxt hd in
  check_stmt_list new_ctxt tl
