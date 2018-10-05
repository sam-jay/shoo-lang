open OUnit2
open Ast

let parse input =
  let lexbuf = Lexing.from_string input in
  Parser.program Scanner.token lexbuf

let empty_prog test_ctxt = assert_equal [] (parse "")

let int_lit test_ctxt = assert_equal [Expr(IntLit(5))] (parse "5;")

let mandatory_semi test_ctxt =
  let f = fun () -> parse "5" in
  assert_raises Parsing.Parse_error f

let comment_test1 test_ctxt = assert_equal [Expr(IntLit(5)); Expr(IntLit(6))] (parse "5;/* this is a \n5; multiline \n comment */6;")

let comment_test2 test_ctxt = assert_equal [Expr(IntLit(5)); Expr(IntLit(6))] (parse "5;/* this is a /* once \n 5; nested */ multiline \n comment */6;")

let comment_test3 test_ctxt = assert_equal [Expr(IntLit(5)); Expr(IntLit(6))] (parse "5;/* this is a /* /* twice \n 5; */ \n nested */ multiline \n comment */6;")

let linec_test test_ctxt = assert_equal [Expr(IntLit(5)); Expr(IntLit(6))] (parse "5; // this is a comment \n 6;")

let comment_tests =
  "Comments" >:::
  [
    "Should accept multiline comment" >:: comment_test1;
    "Should accept once nested multiline comment" >:: comment_test2;
    "Should accept twice nested multiline comment" >:: comment_test3;
    "Should handle single line comment" >:: linec_test;
  ]

let float_test1 test_ctxt = assert_equal [Expr(FloatLit(0.1234))] (parse "0.1234;")
let float_test2 test_ctxt = assert_equal [Expr(FloatLit(0.1234))] (parse ".1234;")

let float_tests =
  "Floating point numbers" >:::
  [
    "Should accept positive with leading 0" >:: float_test1;
    "Should accept positive with leading 0 omitted" >:: float_test2;
  ]

let int_dec test_ctxt = assert_equal [VDecl(Int, "x")] (parse "int x;")
let int_def test_ctxt = assert_equal [VDef(Int, "x", IntLit(5))] (parse "int x = 5;")
let vdec_tests =
  "Variable declarations and definitions" >:::
  [
    "Should handle declaration of int" >:: int_dec;
    "Should handle definition of int" >:: int_def;
  ]

(* TODO(claire) someone needs to write a test for assignment *)

(* TODO(claire) need to implement this once minus is implemented *)
(*let for_all = assert_equal [ForLoop(VDef(Int, "x", IntLit(0)), true 
  (* TODO(claire): change this one > and < are implemented *), Assign("x", Expr*)  
let for_no_init_no_imp test_ctxt = assert_equal [ForLoop( NoExpr, BoolLit(true) (* TODO(claire): change this one once have <> *), 
  NoExpr, [Expr(IntLit(5))])] (parse "for ( ; true; ) { 5; }") 

let for_no_test test_ctxt =
  let f = fun () -> parse "for (; ; ) {5;}" in
  assert_raises Parsing.Parse_error f

let for_tests = 
    "For loops" >:::
    [
(*      "Should handle for loop with initialization, testing, and increment"
        >:: for_all;*) (* TODO(claire) impelement this one math is implemented *)
      (*"Should handle for with missing initialization" >:: for_no_init;
      "Should handle for with missing increment" >:: for_no_increment;*)
      (* TODO(claire) ^^^^ implement after math is impelemented *)
      "Should handle for loop with missing init and increment" >:: for_no_init_no_imp;
      "Should raise error if no test" >:: for_no_test;
    ]

let tests =
  "Parser" >:::
  [
    "Should accept empty program" >:: empty_prog;
    "Should accept int literal" >:: int_lit;
    "Semicolon should be mandatory" >:: mandatory_semi;
    comment_tests;
    float_tests;
    vdec_tests;
    for_tests;
  ]
