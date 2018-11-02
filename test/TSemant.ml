open OUnit2
open Ast

let check input =
  let lexbuf = Lexing.from_string input in
  let program = Parser.program Scanner.token lexbuf in
  let _ = Semant.check_program program in
  ""

(* Variable Reference *)

let ref_int_undec test_ctxt =
  let f = fun () -> check "int x = 1; x + y;" in
  assert_raises (Semant.Undeclared_reference "undeclared reference") f
  

(* Variable Assignment *)

let asn_int_int test_ctxt = assert_equal "" (check "int x; x = 5;")
let asn_bool_bool test_ctxt = assert_equal "" (check "bool b; b = false;")
let asn_str_str test_ctxt = assert_equal "" (check "string s; s = \"abs\";")

let asn_int_str test_ctxt =
  let f = fun () -> check "int x; x = \"foo\";" in
  assert_raises (Semant.Type_mismatch "type mismatch in assignment") f

let asn_int_bool test_ctxt =
  let f = fun () -> check "int i; i = true;" in
  assert_raises (Semant.Type_mismatch "type mismatch in assignment") f

let asn_bool_int test_ctxt =
  let f = fun () -> check "bool b; b = 48;" in
  assert_raises (Semant.Type_mismatch "type mismatch in assignment") f


(* Binary Operators *)
let binop_int_int test_ctxt = assert_equal "" (check "int x = 1; int y = 2; x + y;")

let binop_bool_int test_ctxt =
  let f = fun () -> check "bool b = true; int x = 3; b + x; " in
  assert_raises (Semant.Type_mismatch "Type mismatch across binary operator") f


(* Unary Operators *)
let unop_neg_int test_ctxt = assert_equal "" (check "int x = 1; int y = -x;")
let unop_not_bool test_ctxt = assert_equal "" (check "bool x = true; bool y = !x;")

let unop_not_int test_ctxt =
  let f = fun () -> check "int x = 1; int y = !x; " in
  assert_raises (Semant.Type_mismatch "Type mismatch for unary operator") f
  
let unop_neg_str test_ctxt =
  let f = fun () -> check "string x = \"str\"; bool y = -x; " in
  assert_raises (Semant.Type_mismatch "Type mismatch for unary operator") f

(* Postfix Unary Operators *)
let unop_inc_int test_ctxt = assert_equal "" (check "int x = 1; int y = 3 + x++;")

let unop_dec_str test_ctxt =
  let f = fun () -> check "string x = \"str\"; bool y = x--; " in
  assert_raises (Semant.Type_mismatch "Type mismatch for unary operator") f
  

(* If Statement *)

let if_stat_empty test_ctxt = assert_equal "" (check "if (true) {} ")
let if_stat_empty_else test_ctxt = assert_equal "" (check "if (false) {} else {} ")


(* Function Declaration *)

let func_dec_int test_ctxt = assert_equal "" (check "function foo() int { return 0; }")

let fcall_valid test_ctxt = assert_equal "" (check "function foo() int { return 0; } foo();")

let fcall_invalid test_ctxt =
  let f = fun () -> check "foo();" in
  assert_raises (Semant.Undeclared_reference "undeclared function foo") f

let assign_to_global test_ctxt = assert_equal "" (check "
string x;
function foo() int {
  x = \"foo\";
  return 1;
}
foo();
println(x);")

let shadow_global test_ctxt = assert_equal "" (check "
int x;
function foo() int {
  int x;
  x = 5;
  return 1;
}")

let tests =
  "Semantic checker" >:::
  [
    (* Variable Reference *)
    "Undeclared int" >:: ref_int_undec;
  
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
    
    (* Unary Operators *)
    "Unop for int negation" >:: unop_neg_int;
    "Unop for bool negation" >:: unop_not_bool;
    "Unop for not int" >:: unop_not_int;
    "Unop for string negation" >:: unop_neg_str;
    
    (* Postfix Unary Operators *)
    "Unop for int increment" >:: unop_inc_int;
    "Unop for string decrement" >:: unop_dec_str;
    
    (* If Statement *)
    "If statement with empty block" >:: if_stat_empty;
    "If statement with empty block and an else" >:: if_stat_empty_else;
    
    (* Function Declaration *)
    "Function declaration that returns int" >:: func_dec_int;

    "Valid function call" >:: fcall_valid;
    "Missing function call" >:: fcall_invalid;

    "Assign to a global variable inside a function" >:: assign_to_global;
    "Shadow a global variable inside a function" >:: shadow_global;
  ]
