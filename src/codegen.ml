module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let declare_ext_functions context the_module =
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and void_t     = L.void_type   context in

  let println_t : L.lltype = 
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let println_func : L.llvalue = 
    L.declare_function "printf" println_t the_module in
  StringMap.add "printf" println_func StringMap.empty

let gen_program context the_module program functions =
  
  let builder = L.builder context in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and str_t      = L.pointer_type (L.i8_type context)
  and void_t     = L.void_type   context in

  (* Return the LLVM type for a Shoo type *)
  let ltype_of_typ = function
      A.Int -> i32_t
    | _  -> void_t
  in

  let declare_func_main =
    let main_t = L.function_type i32_t (Array.of_list []) in
    let main = L.declare_function "main" main_t the_module in
    let bb = L.append_block context "entry" main in
    L.position_at_end bb builder;

    let rec expr (sx_t, sx) = match sx with
    | SStrLit s -> L.build_global_stringptr s "str" builder
    | SFCall("println", [(sx_t, sx)]) ->
        let string_format_str = L.build_global_stringptr "%s\n" "fmt" builder in
        let printf_func = StringMap.find "printf" functions in
        L.build_call printf_func [| string_format_str; (expr (sx_t, sx)); |] "" builder
    | SFCall(n, args) ->
        let func = StringMap.find n functions in
        let llargs = List.rev (List.map expr (List.rev args)) in
        L.build_call func (Array.of_list llargs) "" builder
    | _ -> L.const_int i32_t 0
    in

    let gen_sstmt = function
      SExpr(se) -> expr se
    | _ -> L.const_int i32_t 0
    in

    let rec gen_sstmt_list = function
      [] -> ()
    | hd::tl -> gen_sstmt hd; gen_sstmt_list tl;
    in
    gen_sstmt_list program;

    let ret_val = L.const_int i32_t 0 in
    let _ = L.build_ret ret_val builder in
    ()
  in

  declare_func_main

let translate program = 

  let context = L.global_context () in
  let the_module = L.create_module context "Shoo" in

  let functions = declare_ext_functions context the_module in
  gen_program context the_module program functions;

  the_module
