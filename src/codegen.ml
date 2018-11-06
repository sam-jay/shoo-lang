module L = Llvm
open Ast
open Sast
open Lift

module StringMap = Map.Make(String)

let translate functions =
  let context = L.global_context () in
  
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and void_t     = L.void_type   context
  and void_ptr_t = L.pointer_type (L.i8_type context)
  and str_t      = L.pointer_type (L.i8_type context)

  and the_module = L.create_module context "Shoo" in

  let rec ltype_of_func name (ret_t : typ) param_ts =
    let param_types = (List.map ltype_of_typ param_ts) in
    let param_types =
      if name = "main" then param_types
      else void_ptr_t :: param_types
    in L.function_type (ltype_of_typ ret_t) (Array.of_list param_types)

  and ltype_of_lfexpr name (lfexpr : lfunc) =
    ltype_of_func name lfexpr.lreturn_typ (List.map fst lfexpr.lparams)

  and ltype_of_sfunction name (sfunc : func_typ) =
    ltype_of_func name sfunc.return_typ sfunc.param_typs

  and ltype_of_clsr name lfexpr =
    let func_t = L.pointer_type (ltype_of_lfexpr name lfexpr) in
    L.struct_type context [|func_t; void_ptr_t|]
  
  and ltype_of_clsr_func name (sfunc : func_typ) =
    let func_t = L.pointer_type (ltype_of_sfunction name sfunc) in
    L.struct_type context [|func_t; void_ptr_t|]
  
  and ltype_of_typ = function
    Int -> i32_t
  | Float -> float_t
  | Bool -> i1_t
  | String -> str_t
  | Func ftype -> ltype_of_clsr_func "" ftype
  | Void -> void_t
  | _ -> raise (Failure "not yet implemented")
  in

  let insert_value builder agg i v = L.build_insertvalue agg v i "tmp__" builder in

  let typ_of_lfexpr lfexpr = Func({
    return_typ = lfexpr.lreturn_typ;
    param_typs = List.map fst lfexpr.lparams
  }) in

  let rec generate_seq n = if n >= 0 then (n :: (generate_seq (n-1))) else [] in

  let printf_func : L.llvalue =
    let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    L.declare_function "printf" printf_t the_module in
  
  let str_of_int_func : L.llvalue =
    let str_of_int_t = L.function_type (L.pointer_type i8_t) [| i32_t |] in
    L.declare_function "str_of_int" str_of_int_t the_module in

  (* Build each function signature without building the body *)
  let function_decls : (L.llvalue * lfunc) StringMap.t =
    let function_decl m (name, lfexpr) =
      let ftype = ltype_of_lfexpr name lfexpr in
      StringMap.add name (L.define_function name ftype the_module, lfexpr) m 
    in List.fold_left function_decl StringMap.empty functions
  in

  (* Fill in the body for a given function *)
  let build_function_body (name, lfexpr) =
    let (the_function, _) = StringMap.find name function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let string_format_str = L.build_global_stringptr "%s\n" "fmt" builder
    (*and int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder
    and char_format_str = L.build_global_stringptr "%c\n" "fmt" builder*)
    in

    (* Unpacking args and env vars,
      skip for main as its env is empty and has no params *)
    let local_vars =
      let add_param m (t, n) p =
        let () = L.set_value_name n p in
        let local = L.build_malloc (ltype_of_typ t) n builder in
        let _ = L.build_store p local builder in
        StringMap.add n (t, local) m
      in
      let param_list = Array.to_list (L.params the_function) in
      let params =
        if List.length param_list <= 1 then StringMap.empty
        else List.fold_left2 add_param StringMap.empty lfexpr.lparams (List.tl param_list)
      in

      let env =
        if param_list = []
        then L.const_null void_ptr_t
        else List.hd param_list
      in

      let () = L.set_value_name "env" env in
      let env_void_ptr = L.build_malloc void_ptr_t "env" builder in
      let _ = L.build_store env env_void_ptr builder in
      let env_p = L.build_load env_void_ptr "env_p" builder in

      let params_of_lfexpr lfexpr = match lfexpr.lfvs with
        [] -> params
      | _ ->
        let ptr_of_fv (t, _) = L.pointer_type (ltype_of_typ t) in
        let env_struct = L.struct_type context (Array.of_list (List.map ptr_of_fv lfexpr.lfvs)) in
        let env_ptr_t = L.pointer_type env_struct in
        let env_ptr = L.build_bitcast env_p env_ptr_t "env_p" builder in
        let env_val = L.build_load env_ptr "env_val" builder in
        let add_free_var m (t, n) idx =
          let free_var = L.build_extractvalue env_val idx "tmp_" builder in
          StringMap.add n (t, free_var) m
        in
        let fvs_count = List.length lfexpr.lfvs in
        List.fold_left2 add_free_var params lfexpr.lfvs (List.rev (generate_seq (fvs_count - 1)))
      in

      let params_fvs = match name with
        "main" -> params
      | _ -> params_of_lfexpr lfexpr
      in

      (* Allocate a closure of the function within itself for recursive calls *)
      let clsr_t = ltype_of_clsr name lfexpr in
      let clsr_p = L.build_malloc clsr_t lfexpr.lname builder in
      let clsr_val = List.fold_left2 (insert_value builder) (L.const_null clsr_t) [0;1] [the_function;env_p] in
      let _ = L.build_store clsr_val clsr_p builder in
      let func_t  = typ_of_lfexpr lfexpr in
      StringMap.add lfexpr.lname (func_t, clsr_p) params_fvs
    in

    let rec expr builder (m : (typ * L.llvalue) StringMap.t) ((ty, e) : sexpr) =

      let lookup_both n = try StringMap.find n m with
        Not_found -> raise (Failure ("Variable not found: " ^ n)) 
      in

      let lookup n =
        let (_, llval) = try StringMap.find n m with
          Not_found -> raise (Failure ("Variable not found: " ^ n))
        in llval
      in

      let build_clsr clsr =
        let fvs = List.map snd clsr.free_vars in
        let llfvs = List.map lookup fvs in
        let fvs_t = List.map ltype_of_typ (List.map fst clsr.free_vars) in
        let fvs_ptr_t = List.map L.pointer_type fvs_t in
        let env_struct_t = L.struct_type context (Array.of_list fvs_ptr_t) in
        let env_struct = L.build_malloc env_struct_t "tmp_" builder in
        let idxs = List.rev (generate_seq ((List.length fvs) - 1)) in
        let env_val = List.fold_left2 (insert_value builder) (L.const_null env_struct_t) idxs llfvs in
        let _ = L.build_store env_val env_struct builder in
        let env_struct_p = L.build_bitcast env_struct void_ptr_t "env_p" builder in

        (* Pack the function ptr and the env ptr into the closure struct *)
        let func_name = "f" ^ (string_of_int clsr.ind) in
        let (llfunc, sfexpr) = StringMap.find func_name function_decls in
        let llclosure_struct_t = ltype_of_clsr func_name sfexpr in
        let clsr_val = List.fold_left2 
          (insert_value builder) 
          (L.const_null llclosure_struct_t)
          [0;1] 
          [llfunc; env_struct_p]
        in clsr_val
      in
      
      match e with
        SStrLit s -> L.build_global_stringptr s "str" builder
      | SIntLit x -> L.const_int i32_t x
      | SFloatLit x -> L.const_float_of_string float_t x
      | SId s -> L.build_load (lookup s) s builder
      | SNoexpr -> L.const_int i32_t 0
      | SClosure clsr -> build_clsr clsr
      | SFCall((_, SId("println")), [(typ, sexpr)]) ->
          L.build_call printf_func [| string_format_str; (expr builder m (typ, sexpr)); |] "" builder
      | SFCall((_, SId("str_of_int")), [arg]) ->
          L.build_call str_of_int_func [| (expr builder m arg) |] "_result" builder
      | SFCall((t, s), args) ->
          let func_t = match t with
            Func(func_t) -> func_t
          | _ -> raise (Failure "wrong type for function call") in
          let clsr_val = expr builder m (t, s) in
          let func_ptr = L.build_extractvalue clsr_val 0 "fp" builder in
          let env_ptr = L.build_extractvalue clsr_val 1 "envp" builder in
          let llargs = env_ptr :: (List.rev (List.map (expr builder m) (List.rev args))) in
          let result = (match func_t.return_typ with Void -> "" | _ -> "_result") in
          L.build_call func_ptr (Array.of_list llargs) result builder
      | _ as x -> print_endline(fmt_sexpr (ty, x));  raise (Failure "not implemented in codegen")
    in

    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        None -> ignore (instr builder)
      | Some _ -> ()
    in

    let rec stmt builder m = function
      SExpr e -> let _ = expr builder m e in (builder, m)
    | SVDecl(t, n, Some(e)) ->
        let e' = expr builder m e in
        let alloc_clsr clsr =
          let func_name = ("f" ^ (string_of_int clsr.ind)) in
          let (_, lfexpr) = StringMap.find func_name function_decls in
          let func_t = L.pointer_type (ltype_of_lfexpr func_name lfexpr) in
          let llclosure_struct_t = L.struct_type context [|func_t; void_ptr_t|] in
          L.build_malloc llclosure_struct_t n builder
        in
        let (_, expr) = e in
        let local_var = match expr with
          SClosure(clsr) -> alloc_clsr clsr
        | _ -> L.build_malloc (ltype_of_typ t) n builder
        in
        let m' = StringMap.add n (t, local_var) m in
        let _ = L.build_store e' local_var builder in
        (builder, m')
    | SReturn e ->
        let _ = match lfexpr.lreturn_typ with
          Void -> L.build_ret_void builder
        | _ -> L.build_ret (expr builder m e) builder
        in (builder, m)
    | _ -> raise (Failure "not implemented in codegen")

    and stmt_list builder m sl =
      (* throw away the scope generated because it should not be modifying current scope.
      does this need to change? *)
      let helper (bldr, map) = stmt bldr map in
      let (b, _) = List.fold_left helper (builder, m) sl in
      (b, m)
    in

    let (builder, _) = stmt_list builder local_vars lfexpr.lbody in

    (* add a return if the last block falls off the end *)
    add_terminal builder (match lfexpr.lreturn_typ with
      Void -> L.build_ret_void
    | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))

  in
  List.iter build_function_body functions;
  the_module