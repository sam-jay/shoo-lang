open OUnit2
open Ast

let check input =
  let lexbuf = Lexing.from_string input in
  let program = Parser.program Scanner.token lexbuf in
  let _ = Semant.check_program program in
  ""

(* Variable Assignment *)

let asn_int_int test_ctxt = assert_equal "" (check "int x; x = 5;")
let asn_bool_bool test_ctxt = assert_equal "" (check "bool b; b = false;")
let asn_str_str test_ctxt = assert_equal "" (check "string s; s = \"abs\";")

let asn_int_str test_ctxt =
  let f = fun () -> check "int x; x = \"foo\";" in
  assert_raises (Semant.Invalid_assignment "type mismatch in assignment") f

let asn_int_bool test_ctxt =
  let f = fun () -> check "int i; i = true;" in
  assert_raises (Semant.Invalid_assignment "type mismatch in assignment") f

let asn_bool_int test_ctxt =
  let f = fun () -> check "bool b; b = 48;" in
  assert_raises (Semant.Invalid_assignment "type mismatch in assignment") f


(* Binary Operators *)
let binop_int_int test_ctxt = assert_equal "" (check "int x = 1; int y = 2; x + y;")


let binop_bool_int test_ctxt =
  let f = fun () -> check "bool b = true; int x = 3; b + x; " in
  assert_raises (Semant.Illegal_binary_operator "illegal binary operator") f


(* If Statement *)

let if_stat_empty test_ctxt = assert_equal "" (check "if (true) {} ")
let if_stat_empty_else test_ctxt = assert_equal "" (check "if (false) {} else {} ")


(* Function Declaration *)

let func_dec_int test_ctxt = assert_equal "" (check "function int main() { return 0; }")



let tests =
  "Semantic checker" >:::
  [
    (* Variable Assignment *)
    "Int to int assignment" >:: asn_int_int;
    "Bool to bool assignment" >:: asn_bool_bool;
    "String to string assignment" >:: asn_str_str;
    "String to int assignment" >:: asn_int_str;
    "Bool to int assignment" >:: asn_int_bool;
    "Int to bool assignment" >:: asn_bool_int;
    
    (* Binary Operators *)
    "Binop between int and int" >:: binop_int_int;
    "Binop between bool and int" >:: binop_bool_int;
    
    (* If Statement *)
    "If statement with empty block" >:: if_stat_empty;
    "If statement with empty block and an else" >:: if_stat_empty_else;
    
    (* Function Declaration *)
    (* "Function declaration that returns int" >:: func_dec_int; *)
  ]
