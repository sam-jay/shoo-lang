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

let comment_tests =
  "Comments" >:::
  [
    "Should accept multiline comment" >:: comment_test1;
    "Should accept once nested multiline comment" >:: comment_test2;
    "Should accept twice nested multiline comment" >:: comment_test3;
  ]

let float_test1 test_ctxt = assert_equal [Expr(FloatLit(0.1234))] (parse "0.1234;")
let float_test2 test_ctxt = assert_equal [Expr(FloatLit(0.1234))] (parse ".1234;")

let float_tests =
  "Floating point numbers" >:::
  [
    "Should accept positive with leading 0" >:: float_test1;
    "Should accept positive with leading 0 omitted" >:: float_test2;
  ]

let tests =
  "Parser" >:::
  [
    "Should accept empty program" >:: empty_prog;
    "Should accept int literal" >:: int_lit;
    "Semicolon should be mandatory" >:: mandatory_semi;
    comment_tests;
    float_tests;
  ]