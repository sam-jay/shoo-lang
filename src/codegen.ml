module L = Llvm
module A = Ast
open Sast

let translate program = 
  let context = L.global_context () in

  let the_module = L.create_module context "Shoo" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and void_t     = L.void_type   context in

  (* Return the LLVM type for a Shoo type *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | _  -> void_t
  in

  let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
      L.declare_function "printf" printf_t the_module in

  let add_terminal builder instr =
  match L.block_terminator (L.insertion_block builder) with
    Some _ -> ()
  | None -> ignore (instr builder) in

  let func_main = L.define_function "main" i32_t the_module in

  let builder = L.builder_at_end context (L.entry_block func_main) in

  L.build_call func_main (Array.of_list []) "main_result" builder;

  add_terminal builder (L.build_ret (L.const_int i32_t 0));

  the_module
