open OUnit2
open Ast

let check input =
  let lexbuf = Lexing.from_string input in
  let program = Parser.program Scanner.token lexbuf in
  let _ = Semant.check_program program in
  ""

(* Tests that should pass *)

let asn_int_int test_ctxt = assert_equal "" (check "int x; x = 5;")
let asn_bool_bool test_ctxt = assert_equal "" (check "bool b; b = false;")
let asn_str_str test_ctxt = assert_equal "" (check "string s; s = \"abs\";")

(* Tests that should fail *)

let asn_int_str test_ctxt =
  let f = fun () -> check "int x; x = \"foo\";" in
  assert_raises (Semant.Invalid_assignment "type mismatch in assignment") f

let asn_int_bool test_ctxt =
  let f = fun () -> check "int i; i = true;" in
  assert_raises (Semant.Invalid_assignment "type mismatch in assignment") f

let asn_bool_int test_ctxt =
  let f = fun () -> check "bool b; b = 48;" in
  assert_raises (Semant.Invalid_assignment "type mismatch in assignment") f


let tests =
  "Semantic checker" >:::
  [
    "Int to int assignment" >:: asn_int_int;
    "Bool to bool assignment" >:: asn_bool_bool;
    "String to string assignment" >:: asn_str_str;
    
    "String to int assignment" >:: asn_int_str;
    "Bool to int assignment" >:: asn_int_bool;
    "Int to bool assignment" >:: asn_bool_int
  ]
