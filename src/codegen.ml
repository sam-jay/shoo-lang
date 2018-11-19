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

  let rec ltype_of_func name (ret_t : styp) param_ts =
    let param_types = (List.map ltype_of_typ param_ts) in
    let param_types =
      if name = "main" then param_types
      else void_ptr_t :: param_types
    in L.function_type (ltype_of_typ ret_t) (Array.of_list param_types)

  and ltype_of_lfexpr name (lfexpr : lfunc) =
    ltype_of_func name lfexpr.lreturn_typ (List.map fst lfexpr.lparams)

  and ltype_of_sfunction name (sfunc : sfunc_typ) =
    ltype_of_func name sfunc.sreturn_typ sfunc.sparam_typs

  and ltype_of_clsr name lfexpr =
    let func_t = L.pointer_type (ltype_of_lfexpr name lfexpr) in
    L.struct_type context [|func_t; void_ptr_t|]
  
  and ltype_of_clsr_func name (sfunc : sfunc_typ) =
    let func_t = L.pointer_type (ltype_of_sfunction name sfunc) in
    L.struct_type context [|func_t; void_ptr_t|]
  
  and ltype_of_typ = function
    SInt -> i32_t
  | SFloat -> float_t
  | SBool -> i1_t
  | SString -> str_t
  | SFunc ftype -> ltype_of_clsr_func "" ftype
  | SVoid -> void_t
  | SStruct(struct_t) ->
      let t_members = List.map (fun (_, (t, _)) -> t) (StringMap.bindings struct_t.smembers) in
      L.struct_type context (Array.of_list (List.map ltype_of_typ t_members))
  | SArray(array_typ) -> L.pointer_type (ltype_of_typ array_typ) 
  | _ -> raise (Failure "not yet implemented")
  in

  let insert_value builder agg i v = L.build_insertvalue agg v i "tmp__" builder in

  let llstore builder lval laddr = 
      let ptr = L.build_pointercast laddr 
        (L.pointer_type (L.type_of lval)) "" builder
      in
 
      (*let store_inst =*) (L.build_store lval ptr builder) (*in*)
      (*(L.string_of_llvalue store_inst);*)
      (*()*)
  in
  let typ_of_lfexpr lfexpr = SFunc({
    sreturn_typ = lfexpr.lreturn_typ;
    sparam_typs = List.map fst lfexpr.lparams
  }) in

  let rec generate_seq n = if n >= 0 then (n :: (generate_seq (n-1))) else [] in

  let printf_func : L.llvalue =
    let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    L.declare_function "printf" printf_t the_module in

  let builtins = List.fold_left (fun m (name, ty) -> 
    let ftype = match ty with SFunc(f) -> f | _ -> (raise (Failure "shouldn't happen")) in
    let params = List.map ltype_of_typ ftype.sparam_typs in
    let ltype = L.function_type (ltype_of_typ ftype.sreturn_typ) (Array.of_list params) in
    StringMap.add name (L.declare_function name ltype the_module) m
  ) StringMap.empty Semant.builtins in

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

    let rec expr builder (m : (styp * L.llvalue) StringMap.t) ((ty, e) : sexpr) =

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
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SFloatLit x -> L.const_float_of_string float_t x
      | SId s -> L.build_load (lookup s) s builder
      | SNoexpr -> L.const_int i32_t 0
      | SAssign((_, SDot((SStruct(struct_t), SId(s)), name)), e2) ->
          let new_v = expr builder m e2 in
          let lhs = expr builder m (SStruct(struct_t), SId(s)) in
          let (* TODO need this? (*llstruct_t*)*) _ 
                = ltype_of_typ (SStruct(struct_t)) in
          let compare_by (n1, _) (n2, _) = compare n1 n2 in
          let members = List.sort compare_by (StringMap.bindings struct_t.smembers) in
          let idxs = List.mapi (fun i (n, _) -> (n, i)) members in
          let idx = snd (List.hd (List.filter (fun (n, _) -> n = name) idxs)) in
          let new_struct = L.build_insertvalue lhs new_v idx name builder in
          ignore(L.build_store new_struct (lookup s) builder); new_v
      | SAssign(e1, e2) ->
          let new_v = expr builder m e2 in
          (match snd e1 with
            SId s -> ignore(L.build_store new_v (lookup s) builder); new_v
          | _ -> raise (Failure ("assignment for " ^ (fmt_sexpr e2) ^ "not implemented in codegen")))
      | SArrayLit(sexpr_list) -> 
        (* below seg faults *)  
        (*let (array_type,_) = List.hd sexpr_list in 
          let llarray_t = ltype_of_typ (SArray(array_type)) in
          let vals = List.map (fun e -> expr builder m e) 
            sexpr_list in
          let idxs = List.rev (generate_seq ((List.length sexpr_list) - 1)) in
          List.fold_left2 (insert_value builder) (L.const_null llarray_t) idxs vals*)

          if List.length sexpr_list = 0
              then raise (Failure "Empty array init is not supported")
          else
                let (array_type, _) = List.hd sexpr_list in
                let llarray_t = ltype_of_typ (SArray(array_type)) in
                let all_elem = List.map (fun e ->
                    expr builder m e) sexpr_list in
                (*let array_type = L.type_of (List.hd all_elem) in*)
                let num_elems = List.length sexpr_list in
                let ptr = L.build_array_malloc (*array_type*) llarray_t
                    (L.const_int i32_t num_elems)
                    ""
                    builder in
                let idxs = List.rev (generate_seq (num_elems - 1))
                in
                let llidxs = List.map (L.const_int i32_t) idxs in
                (*List.fold_left2 (insert_value builder) 
                    (*(L.const_null array_type)*) ptr idxs all_elem*)
                ignore (List.fold_left2 (fun i idx elem -> 
                    (*let ind = L.const_int i32_t i in*)
                    let eptr = L.build_gep ptr [|idx|] "" builder in
                    let cptr = L.build_pointercast eptr 
                        (L.pointer_type ((*ltype_of_typ*) L.type_of elem) (*i32_t*)) "" builder in
                    let store_inst = (L.build_store elem cptr builder) 
                    in store_inst)
                ptr llidxs all_elem); ptr
      | SStructInit(SStruct(struct_t), assigns) ->
          let compare_by (n1, _) (n2, _) = compare n1 n2 in
          let members = List.sort compare_by (StringMap.bindings struct_t.smembers) in
          let llstruct_t = ltype_of_typ (SStruct(struct_t)) in
          let vals = List.map (fun (_, e) -> expr builder m e) (List.sort compare_by assigns) in
          let idxs = List.rev (generate_seq ((List.length members) - 1)) in
          List.fold_left2 (insert_value builder) (L.const_null llstruct_t) idxs vals
      | SDot((SStruct(struct_t), exp), name) ->
          let lhs = expr builder m (SStruct(struct_t), exp) in
          let (* TODO need this? (*llstruct_t*)*) _
             = ltype_of_typ (SStruct(struct_t)) in
          let compare_by (n1, _) (n2, _) = compare n1 n2 in
          let members = List.sort compare_by (StringMap.bindings struct_t.smembers) in
          let idxs = List.mapi (fun i (n, _) -> (n, i)) members in
          let idx = snd (List.hd (List.filter (fun (n, _) -> n = name) idxs)) in
          L.build_extractvalue lhs idx name builder
      | SBinop (e1, op, e2) -> (*Ref: Justin's codegen.ml*)
        let (t, _) = e1
        and e1' = expr builder m e1
        and e2' = expr builder m e2 in
          (match snd e1, snd e2 with
           _ -> (match t with

          SFloat -> (match op with
              Add     -> L.build_fadd
            | Sub     -> L.build_fsub
            | Mult    -> L.build_fmul
            | Div     -> L.build_fdiv
            | Equal   -> L.build_fcmp L.Fcmp.Oeq
            | Neq     -> L.build_fcmp L.Fcmp.One
            | Less    -> L.build_fcmp L.Fcmp.Olt
            | Leq     -> L.build_fcmp L.Fcmp.Ole
            | Greater -> L.build_fcmp L.Fcmp.Ogt
            | Geq     -> L.build_fcmp L.Fcmp.Oge
            | _ ->
              raise (Failure "internal error: semant should have rejected and/or on float")
              ) e1' e2' "tmp" builder
        | SInt -> (match op with
            | Add     -> L.build_add
            | Sub     -> L.build_sub
            | Mult    -> L.build_mul
            | Div     -> L.build_sdiv
            | And     -> L.build_and
            | Or      -> L.build_or
            | Equal   -> L.build_icmp L.Icmp.Eq
            | Neq     -> L.build_icmp L.Icmp.Ne
            | Less    -> L.build_icmp L.Icmp.Slt
            | Leq     -> L.build_icmp L.Icmp.Sle
            | Greater -> L.build_icmp L.Icmp.Sgt
            | Geq     -> L.build_icmp L.Icmp.Sge
            | Mod     -> L.build_srem
              ) e1' e2' "tmp" builder
        | SBool -> (match op with
              And     -> L.build_and
            | Or      -> L.build_or
            | _         -> raise (Failure ("operation " ^ (fmt_op op)
                    ^ " not implemented for type " ^ (fmt_styp t)))
              ) e1' e2' "tmp" builder
        | SString -> (match op with
            Add -> L.build_call (StringMap.find "string_concat" builtins) [| e1'; e2'|] "string_concat" builder
          | Equal -> (L.build_icmp L.Icmp.Ne) (L.const_int i32_t 0)
                (L.build_call (StringMap.find "string_equals" builtins) [| e1'; e2'|] "string_equals" builder) "tmp" builder
          | Neq -> (L.build_icmp L.Icmp.Eq) (L.const_int i32_t 0)
                (L.build_call (StringMap.find "string_equals" builtins) [| e1'; e2'|] "string_equals" builder) "tmp" builder
          | _ -> raise (Failure ("operation " ^ (fmt_op op)
                ^ " not implemented for type " ^ (fmt_styp t))))
        | _ -> (match op with
            Equal -> (L.build_icmp L.Icmp.Eq) e1' e2' "tmp" builder
          | Neq -> (L.build_icmp L.Icmp.Ne) e1' e2' "tmp" builder
          | _ -> raise (Failure ("operation " ^ (fmt_op op)
                ^ " not implemented for type " ^ (fmt_styp t))))
         ))
      | SUnop(op, e) -> (*ref: Justin's LRM*)
        let (t, _) = e in
        let e' = expr builder m e in
          (match op with
          Neg when t = SFloat -> L.build_fneg
        | Neg when t = SInt -> L.build_neg
        | Not when t = SBool -> L.build_not
        | _ -> raise (Failure ("operation " ^ (fmt_uop op) ^ 
          " not implemented for type " ^ (fmt_styp t)))) e' "tmp" builder


      | SClosure clsr -> build_clsr clsr
      | SFCall((_, SId("println")), [(typ, sexpr)]) ->
          L.build_call printf_func [| string_format_str; 
            (expr builder m (typ, sexpr)); |] "" builder
      | SFCall((_, SId(name)), args) when StringMap.mem name builtins ->
          let arg_array = Array.of_list (List.map (fun arg -> expr builder m arg) args) in
          L.build_call (StringMap.find name builtins) arg_array "_result" builder
      | SFCall((t, s), args) ->
          let func_t = match t with
            SFunc(func_t) -> func_t
          | _ -> raise (Failure "wrong type for function call") in
          let clsr_val = expr builder m (t, s) in
          let func_ptr = L.build_extractvalue clsr_val 0 "fp" builder in
          let env_ptr = L.build_extractvalue clsr_val 1 "envp" builder in
          let llargs = env_ptr :: (List.rev (List.map (expr builder m) (List.rev args))) in
          let result = (match func_t.sreturn_typ with SVoid -> "" | _ -> "_result") in
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
    | SStructDef(n, mem) ->
        let struct_t = SStruct({
          sstruct_name = n;
          sincomplete = false;
          smembers = List.fold_left (fun m (t, n, opt_se) -> StringMap.add n (t, opt_se) m) StringMap.empty mem;
        }) in
        (builder, StringMap.add n (struct_t, L.const_int i1_t 0) m)
    | SVDecl(t, n, se) ->
        let alloc_clsr clsr =
          let func_name = ("f" ^ (string_of_int clsr.ind)) in
          let (_, lfexpr) = StringMap.find func_name function_decls in
          let func_t = L.pointer_type (ltype_of_lfexpr func_name lfexpr) in
          let llclosure_struct_t = L.struct_type context [|func_t; void_ptr_t|] in
          L.build_malloc llclosure_struct_t n builder
        in
        let (builder, local_var) = match se with
          None -> 
            let local_var = L.build_malloc (ltype_of_typ t) n builder in
            (builder, local_var)
        | Some(e) -> 
            let (_, ex) = e in
            let local_var = match ex with
              SClosure(clsr) -> alloc_clsr clsr
            | _ -> L.build_malloc (ltype_of_typ t) n builder
            in
            let e' = expr builder m e in
            let _ = L.build_store e' local_var builder in
            (builder, local_var)
        in
        let m' = StringMap.add n (t, local_var) m in
        (builder, m')
      (* Ref: Justin's codegen.ml *)
      | SIf (pred, then_stmts, else_stmts) ->
         let bool_val = expr builder m pred in
         let merge_bb = L.append_block context "merge" the_function in
         let branch_instr = L.build_br merge_bb in
         let then_bb = L.append_block context "then" the_function in
         let (then_builder, _) = stmt_list (L.builder_at_end context then_bb) m then_stmts in
           let () = add_terminal then_builder branch_instr in
         let else_bb = L.append_block context "else" the_function in
         let (else_builder, _) = stmt_list (L.builder_at_end context else_bb) m else_stmts in
         let () = add_terminal else_builder branch_instr in
         let _ = L.build_cond_br bool_val then_bb else_bb builder in
         (L.builder_at_end context merge_bb, m)
    | SReturn e ->
        let _ = match lfexpr.lreturn_typ with
          SVoid -> L.build_ret_void builder
        | _ -> L.build_ret (expr builder m e) builder
        in (builder, m)
    | SForLoop (init, predicate, incr, body) ->
        (* Build a basic block for the init statement. *)
        let init_bb = L.append_block context "init_loop" the_function in
        let (init_builder, m_incr) = 
            (match init with 
                Some(init) -> 
                    stmt (L.builder_at_end context init_bb) m init
                | None -> ((L.builder_at_end context init_bb), m)) 
        in 
		let _ = L.build_br init_bb builder in        

        (* Build a basic block for the condition checking *)
        let pred_bb = L.append_block context "for" the_function in
        
        (* Branch to the predicate to execute the condition from
         * the current block. *)
        let _ = L.build_br pred_bb init_builder in
        let body_bb = L.append_block context "for_body" the_function
        in
        
        (* Don't need to keep the map because the variables declared in
         * the for loop only exist in the for loop. *)   
        let (for_builder, _) = List.fold_left (fun (b_bb, temp_map) s -> 
            let (build, map) = 
                stmt b_bb temp_map s in (build, map)) 
            (* TODO(claire) figure out why body needs List.rev *)
            ((L.builder_at_end context body_bb), m_incr) (List.rev body)
        in

        (* Add the increment to the block only if the block doesn't
         * already have a terminator and it has an increment.*)
        let incr_for_builder = match incr with
            Some(incr) ->
                (let has_incr = 
                    match L.block_terminator 
                        (L.insertion_block for_builder) with
                    None -> (let (new_incr_builder, _) = 
                        stmt for_builder m_incr (SExpr(incr)) in
                        new_incr_builder)
                    | Some _ -> for_builder in has_incr)
             | None -> for_builder in
        let() = add_terminal incr_for_builder (L.build_br pred_bb) in
        
        (* Generate the predicate code in the predicate block *)
        let pred_builder = L.builder_at_end context pred_bb in
        let bool_val = match predicate with 
            Some(predicate) -> let has_predicate = 
                expr pred_builder m_incr predicate in has_predicate
            | None -> let always_true = 
                expr pred_builder m_incr (SBool, SBoolLit(true)) in always_true
         in

        (* Finish the loop *)
        let merge_bb = L.append_block context "merge" the_function in
        let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder
        in
        (* Return m instead of m_incr because the incr variable doesn't
         * exist outside of the scope of this loop. *)
        (L.builder_at_end context merge_bb, m)
        
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
      SVoid -> L.build_ret_void
    | SString -> L.build_ret (L.build_global_stringptr "" "str" builder)
    | SFloat -> L.build_ret (L.const_float_of_string float_t "0.0")
    | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))

  in
  List.iter build_function_body functions;
  the_module
