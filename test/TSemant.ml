open OUnit2
open Ast

let check input =
  let lexbuf = Lexing.from_string input in
  let program = Parser.program Scanner.token lexbuf in
  let _ = Semant.check_program program in
  ""

let asn_int_int test_ctxt = assert_equal "" (check "int x; x = 5;")

let asn_int_str test_ctxt =
  let f = fun () -> check "int x; x = \"foo\";" in
  assert_raises (Semant.Invalid_assignment "type mismatch in assignment") f

let tests =
  "Semantic checker" >:::
  [
    "Int to int assignment" >:: asn_int_int;
    "String to int assignment" >:: asn_int_str
  ]
