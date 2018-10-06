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

let common_tests =
  "Common" >:::
  [
    "Should accept empty program" >:: empty_prog;
    "Should accept int literal" >:: int_lit;
    "Semicolon should be mandatory" >:: mandatory_semi;
  ]

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

let int_dec test_ctxt = assert_equal [VDecl(Int, "x", None)] (parse "int x;")
let int_def test_ctxt = assert_equal [VDecl(Int, "x", Some(IntLit(5)))] (parse "int x = 5;")
let vdec_tests =
  "Variable declarations and definitions" >:::
  [
    "Should handle declaration of int" >:: int_dec;
    "Should handle definition of int" >:: int_def;
  ]

let if_only text_ctxt = 
  assert_equal [If(BoolLit(true),[VDecl(Int, "x")],[])] (parse "if(true){int x;}")
let if_else text_ctxt = 
  assert_equal [If(BoolLit(false),[VDecl(Int, "x")],[VDecl(Int, "y")])] 
  (parse "if(false){int x;}else{int y;}")
let if_elif1 text_ctxt = 
  assert_equal [If(BoolLit(false),[VDecl(Int, "x")],[If(BoolLit(true),[VDecl(Int, "y")],[])])] 
  (parse "if(false){int x;}elif(true){int y;}")
let if_elif1_else text_ctxt = 
  assert_equal [If(BoolLit(false),[VDecl(Int, "x")],[If(BoolLit(false),[VDecl(Int, "y")],[VDecl(Int, "z")])])] 
  (parse "if(false){int x;}elif(false){int y;}else{int z;}")
let if_elif2_else text_ctxt = 
  assert_equal [If(BoolLit(false),[VDecl(Int, "x")],[If(BoolLit(false),[VDecl(Int, "y")],[If(BoolLit(false),[VDecl(Int, "w")],[VDecl(Int, "z")])])])] 
  (parse "if(false){int x;}elif(false){int y;}elif(false){int w;}else{int z;}")
let if_else_tests =
  "If else tests" >:::
  [
    "Should handle if statement by itself" >:: if_only;
    "Should handle if statement with else" >:: if_else;
    "Should handle if statement with elif" >:: if_elif1;
    "Should handle if statement with elif and else" >:: if_elif1_else;
    "Should handle if statement with 2 elifs and else" >:: if_elif2_else;
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

let struct_declare test_ctxt = assert_equal
  [StructDef("Point", [
    (Int, "x", Some(IntLit(0)));
    (Int, "y", Some(IntLit(0)));
    (Int, "z", None) ])]
  (parse "struct Point { int x = 0; int y = 0; int z; }")

let struct_declare_empty test_ctxt = assert_equal
  [StructDef("Empty", [])]
  (parse "struct Empty { }")

let struct_def test_ctxt = assert_equal
  [Expr(Assign("x", StructInit([
    ("foo", IntLit(2));
    ("bar", IntLit(5)) ])))]
  (parse "x = { foo = 2; bar = 5; };")
  
let destruct test_ctxt = assert_equal
  [Expr(Destruct(["x"; "y"; "z"], Id("foo")))]
  (parse "{ x; y; z; } = foo;")

let struct_tests =
  "Structs" >:::
  [
    "Should handle struct declaration" >::struct_declare;
    "Should handle empty struct declaration" >::struct_declare_empty;
    "Should handle struct definition" >::struct_def;
    "Should handle destructuring assignment" >::destruct;
  ]

let enhanced_for_id test_ctxt = assert_equal
  [EnhancedFor(Int, "x", Id("foo"), [Expr(IntLit(5))])]
  (parse "for (int x in foo) { 5; }")
let enhanced_for_array_lit test_ctxt = assert_equal
  [EnhancedFor(Int, "x", ArrayLit([
    IntLit(1);
    IntLit(2);
    IntLit(3);
    IntLit(4);
    IntLit(5)
  ]), [Expr(IntLit(5))])]
  (parse "for (int x in [1, 2, 3, 4, 5]) { 5; }")
let enhanced_for_tests =
  "Enhanced For Loop" >:::
  [
    "Should handle by id" >:: enhanced_for_id;
    "Should handle by array lit" >:: enhanced_for_array_lit
  ]

let one_intarr_decl test_ctxt = assert_equal [VDecl(Array(Int, Fixed(5)), "x", None)] (parse "array<int>[5] x;")
let one_intarr_def test_ctxt = assert_equal
  [VDecl(Array(Int, Fixed(5)), "x", Some(ArrayLit([
    IntLit(5);
    IntLit(4);
    IntLit(3);
    IntLit(2);
    IntLit(1)
  ])))]
  (parse "array<int>[5] x = [5, 4, 3, 2, 1];")
let two_intarr_decl test_ctxt = assert_equal [VDecl(Array(Array(Int, Fixed(5)), Fixed(10)), "x", None)] (parse "array< array<int>[5] >[10] x;")
let two_intarr_def test_ctxt = assert_equal
  [VDecl(Array(Array(Int, Fixed(5)), Fixed(2)), "x", Some(ArrayLit([
    ArrayLit([
      IntLit(1);
      IntLit(2);
      IntLit(3);
      IntLit(4);
      IntLit(5)
    ]);
    ArrayLit([
      IntLit(5);
      IntLit(4);
      IntLit(3);
      IntLit(2);
      IntLit(1)
    ]);
  ])))]
  (parse "array< array<int>[5] >[2] x = [[1,2,3,4,5],[5,4,3,2,1]];")

let array_tests =
  "Arrays" >:::
  [
    "One dimensional int array declaration" >::one_intarr_decl;
    "One dimensional int array definition" >::one_intarr_def;
    "Two dimensional int array declaration" >::two_intarr_decl;
    "Two dimensional int array definition" >::two_intarr_def;
  ]

let prog_one_test test_ctxt = assert_equal
  [FDecl("sampleProgram1", [], Void, [
    VDecl(Array(Array(Int, Fixed(10)), Fixed(2)), "tasks", Some(ArrayLit([
      ArrayLit([
        IntLit(1); IntLit(2); IntLit(3); IntLit(4); IntLit(5);
        IntLit(6); IntLit(7); IntLit(8); IntLit(9); IntLit(10);
      ]);
      ArrayLit([
        IntLit(11); IntLit(12); IntLit(13); IntLit(14); IntLit(15);
        IntLit(16); IntLit(17); IntLit(18); IntLit(19); IntLit(20);
      ])
    ])));
    FDecl("sum", [(Int, "x"); (Int, "y")], Int, [
      VDecl(Int, "result", None);
      Return(Id("result"));
    ]);
    FDecl("foldl", [(Func, "f"); (Any, "acc"); (Array(Any, Param("S")), "items")], Array(Any, Param("S")), [
      
    ]);
    FDecl("map", [(Func, "f"); (Array(Any, Param("S")), "items")], Array(Any, Param("S")), [

    ]);
    (*Expr(Assign());
    FCall();*)
  ])]
  (parse "function sampleProgram1() void {
    array< array<int>[10] >[2] tasks = [
      [1,2,3,4,5,6,7,8,9,10],
      [11,12,13,14,15,16,17,18,19,20]
    ];

    function sum(int x, int y) int {
      int result;
      // TODO(sam): calculate result after we have + operator 
      return result;
    }

    function foldl(func f, any acc, array<any>[S] items) array<any>[S] {
      /*if (isEqual(length(items), 0)) {
        return acc;
      } else {
        return foldl(f, f(acc, first(items)), rest(items));
      }*/
    }

    function map(func f, array<any>[S] items) array<any>[S] {
      /*// TODO(sam): turn this into tail recursion
      if (isEqual(length(items), 0)) {
        return [];
      } else {
        return concat(f(first(items)), map(f, rest(items)));
      }*/
    }

    //array<int>[2] results = map(function (task) { return foldl(sum, 0, task); }, tasks);

    //print(string_of_int(foldl(sum, 0, results)))

  }")

let full_prog_tests =
  "Full Programs" >:::
  [
    "Program 1" >:: prog_one_test
  ]

let tests =
  "Parser" >:::
  [
    common_tests;
    comment_tests;
    float_tests;
    vdec_tests;
    if_else_tests;
    for_tests;
    struct_tests;
    enhanced_for_tests;
    array_tests;
    full_prog_tests;
  ]
