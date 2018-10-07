open OUnit2
open Ast

let parse input =
  let lexbuf = Lexing.from_string input in
  Parser.program Scanner.token lexbuf

let empty_prog test_ctxt = assert_equal [] (parse "")

let int_lit test_ctxt = assert_equal [Expr(IntLit(5))] (parse "5;")

let arithmetic_lit_test1 test_ctxt = assert_equal [Expr(Binop(IntLit(82), Add, IntLit(3)))] (parse "82+3;")

let arithmetic_lit_test2 test_ctxt = assert_equal [Expr(Binop(IntLit(82), Sub, IntLit(3)))] (parse "82-3;")

let arithmetic_lit_test3 test_ctxt = assert_equal [Expr(Binop(IntLit(82), Mult, IntLit(3)))] (parse "82*3;")

let arithmetic_lit_test4 test_ctxt = assert_equal [Expr(Binop(IntLit(82), Div, IntLit(3)))] (parse "82/3;")

let arithmetic_lit_test5 test_ctxt = assert_equal [Expr(Binop(IntLit(82), Add, Binop(IntLit(2), Mult, IntLit(4))))] (parse "82+2*4;")

let arithmetic_tests =
  "Arithmetic operations" >:::
  [
    "Should accept addition" >:: arithmetic_lit_test1;
    "Should accept subtraction" >:: arithmetic_lit_test2;
    "Should accept multiplication" >:: arithmetic_lit_test3;
    "Should accept division" >:: arithmetic_lit_test4;
    "Should accept combination of operations" >:: arithmetic_lit_test5;
  ]

let logical_lit_test1 test_ctxt = assert_equal [Expr(Unop(Neg, IntLit(4)))] (parse "-4;")

let logical_lit_test2 test_ctxt = assert_equal [Expr(Binop(IntLit(5), Greater, IntLit(3)))] (parse "5>3;")

let logical_lit_test3 test_ctxt = assert_equal [Expr(Binop(IntLit(1), Or, IntLit(2)))] (parse "1||2;")

let logical_lit_test4 test_ctxt = assert_equal [Expr(Binop(IntLit(6), Leq, IntLit(8)))] (parse "6<=8;")

let logical_lit_test5 test_ctxt = assert_equal [Expr(Binop(Binop(IntLit(82), Add, IntLit(3)), Leq, IntLit(90)))] (parse "82+3<=90;")
let logical_lit_test6 test_ctxt = assert_equal [Expr(Binop(IntLit(85), Equal, IntLit(85)))] (parse "85==85;")

let logical_tests =
  "Logical operations" >:::
  [
    "Should accept negation" >:: logical_lit_test1;
    "Should accept greater sign" >:: logical_lit_test2;
    "Should accept or" >:: logical_lit_test3;
    "Should accept less than or equal to" >:: logical_lit_test4;
    "Should accept logical operations in order" >:: logical_lit_test5;
    "Should accept logical equals" >:: logical_lit_test6;
  ]

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
  assert_equal [If(BoolLit(true),[VDecl(Int, "x", None)],[])] (parse "if(true){int x;}")
let if_else text_ctxt = 
  assert_equal [If(BoolLit(false),[VDecl(Int, "x", None)],[VDecl(Int, "y", None)])] 
  (parse "if(false){int x;}else{int y;}")
let if_elif1 text_ctxt = 
  assert_equal [If(BoolLit(false),[VDecl(Int, "x", None)],[If(BoolLit(true),[VDecl(Int, "y", None)],[])])] 
  (parse "if(false){int x;}elif(true){int y;}")
let if_elif1_else text_ctxt = 
  assert_equal [If(BoolLit(false),[VDecl(Int, "x", None)],[If(BoolLit(false),[VDecl(Int, "y", None)],[VDecl(Int, "z", None)])])] 
  (parse "if(false){int x;}elif(false){int y;}else{int z;}")
let if_elif2_else text_ctxt = 
  assert_equal [If(BoolLit(false),[VDecl(Int, "x", None)],[If(BoolLit(false),[VDecl(Int, "y", None)],[If(BoolLit(false),[VDecl(Int, "w", None)],[VDecl(Int, "z", None)])])])] 
  (parse "if(false){int x;}elif(false){int y;}elif(false){int w;}else{int z;}")
let if_elif2_else_true text_ctxt = 
  assert_equal [If(BoolLit(true),[VDecl(Int, "x", None)],[If(BoolLit(false),[VDecl(Int, "y", None)],[If(BoolLit(false),[VDecl(Int, "w", None)],[VDecl(Int, "z", None)])])])] 
  (parse "if(true){int x;}elif(false){int y;}elif(false){int w;}else{int z;}")
let if_else_tests =
  "If else tests" >:::
  [
    "Should handle if statement by itself" >:: if_only;
    "Should handle if statement with else" >:: if_else;
    "Should handle if statement with elif" >:: if_elif1;
    "Should handle if statement with elif and else" >:: if_elif1_else;
    "Should handle if statement with 2 elifs and else" >:: if_elif2_else;
    "Should handle if statement with 2 elifs and else while true" >:: if_elif2_else_true;
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

let get_struct_val test_ctxt = assert_equal
  [Expr(Id("myStruct.x"))]
  (parse "myStruct.x;")

let set_struct_val test_ctxt = assert_equal
  [Expr(Assign("myStruct.x", IntLit(5)))]
  (parse "myStruct.x = 5;")

let toy_struct_program test_ctxt = assert_equal
  [
    StructDef("BankAccount", [(Int, "balance", None); (Int, "ownerId", None)]);
    VDecl(Struct("BankAccount"), "myAccount", Some(New(NStruct("BankAccount"))));
    Expr(Assign("myAccount.balance", IntLit(0)));
    Expr(Assign("myAccount.ownerId", IntLit(0)));
  ]
  (parse "struct BankAccount { int balance; int ownerId; }
  BankAccount myAccount = new(BankAccount);
  myAccount.balance = 0; myAccount.ownerId = 0;
  ")

let struct_tests =
  "Structs" >:::
  [
    "Should handle struct declaration" >::struct_declare;
    "Should handle empty struct declaration" >::struct_declare_empty;
    "Should handle struct definition" >::struct_def;
    "Should handle destructuring assignment" >::destruct;
    "Getting struct value" >::get_struct_val;
    "Setting struct value" >::set_struct_val;
    "Toy struct program" >::toy_struct_program;
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

let one_intarr_decl test_ctxt = assert_equal [VDecl(Array(Int), "x", None)] (parse "array<int> x;")
let one_intarr_def test_ctxt = assert_equal
  [VDecl(Array(Int), "x", Some(ArrayLit([
    IntLit(5);
    IntLit(4);
    IntLit(3);
    IntLit(2);
    IntLit(1)
  ])))]
  (parse "array<int> x = [5, 4, 3, 2, 1];")
let two_intarr_decl test_ctxt = assert_equal [VDecl(Array(Array(Int)), "x", None)] (parse "array< array<int> > x;")
let two_intarr_def test_ctxt = assert_equal
  [VDecl(Array(Array(Int)), "x", Some(ArrayLit([
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
  (parse "array< array<int> > x = [[1,2,3,4,5],[5,4,3,2,1]];")

let array_tests =
  "Arrays" >:::
  [
    "One dimensional int array declaration" >::one_intarr_decl;
    "One dimensional int array definition" >::one_intarr_def;
    "Two dimensional int array declaration" >::two_intarr_decl;
    "Two dimensional int array definition" >::two_intarr_def;
  ]

let new_one_array test_ctxt = assert_equal [Expr(New(NArray(Int, IntLit(5))))] (parse "new(array<int>[5]);")
let new_struct test_ctxt = assert_equal [Expr(New(NStruct("BankAccount")))] (parse "new(BankAccount);")


let new_tests =
  "New keyword" >:::
  [
    "New one dimensional array" >::new_one_array;
    "New struct" >::new_struct;
  ]

let prog_one_test test_ctxt = assert_equal
  [FDecl("sampleProgram1", [], Void, [
    VDecl(Array(Array(Int)), "tasks", Some(ArrayLit([
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
      Return(Binop(Id("x"), Add, Id("y")));
    ]);
    FDecl("foldl", [(Func, "f"); (Any, "acc"); (Array(Any), "items")], Array(Any), [
      If(Binop(FCall("length", [Id("items")]), Equal, IntLit(0)),
        [Return(Id("acc"))],
        [Return(FCall("foldl", [Id("f"); FCall("f", [Id("acc"); FCall("first", [Id("items")])]); FCall("rest", [Id("items")])]))])
    ]);
    FDecl("map", [(Func, "f"); (Array(Any), "items")], Array(Any), [
      If(Binop(FCall("length", [Id("items")]), Equal, IntLit(0)),
        [Return(ArrayLit([]))],
        [Return(FCall("concat",[
          FCall("f", [FCall("first", [Id("items")])]);
          FCall("map", [Id("f"); FCall("rest", [Id("items")])])]))])
    ]);
    VDecl(Array(Int), "results", Some(FCall("map", [
      FExpr([(Array(Int), "task")],
        Array(Int),
        [Return(FCall("foldl", [Id("sum"); IntLit(0); Id("task")]))]);
      Id("tasks")
    ])));
    Expr(FCall("print", [FCall("stringOfInt", [FCall("foldl", [Id("sum"); IntLit(0); Id("results")])])]));
  ])]
  (parse "function sampleProgram1() void {
    array<array<int>> tasks = [
      [1,2,3,4,5,6,7,8,9,10],
      [11,12,13,14,15,16,17,18,19,20]
    ];

    function sum(int x, int y) int {
      return x + y;
    }

    function foldl(func f, any acc, array<any> items) array<any> {
      if (length(items) == 0) {
        return acc;
      } else {
        return foldl(f, f(acc, first(items)), rest(items));
      }
    }

    function map(func f, array<any> items) array<any> {
      // TODO(sam): turn this into tail recursion
      if (length(items) == 0) {
        return [];
      } else {
        return concat(f(first(items)), map(f, rest(items)));
      }
    }

    array<int> results = map(function (array<int> task) array<int> { return foldl(sum, 0, task); }, tasks);

    print(stringOfInt(foldl(sum, 0, results)));

  }")

let prog_two_test test_ctxt = assert_equal
[FDecl("sampleProgram2", [], Void, [

  StructDef("BankAccount", [(Int, "balance", None); (Int, "ownerId", None)]);
  VDecl(Struct("BankAccount"), "myAccount", Some(New(NStruct("BankAccount"))));
  Expr(Assign("myAccount.balance", IntLit(0)));
  Expr(Assign("myAccount.ownerId", IntLit(12345)));

  VDecl(Array(Int), "quantities", Some(ArrayLit([
      IntLit(500); IntLit(200); IntLit(1400); IntLit(3000); IntLit(1000);
  ])
  ));
  FDecl("deposit", [(Struct("BankAccount"), "act"); (Int, "amount")], Int, [
    Expr(Assign("act.balance", Binop(Id("act.balance"), Add, Id("amount"))));
    Return(Id("act.balance"));
  ]);
  FDecl("withdraw", [(Struct("BankAccount"), "act"); (Int, "amount")], Int, [
    Expr(Assign("act.balance", Binop(Id("act.balance"), Sub, Id("amount"))));
    Return(Id("act.balance"));
  ]);
  EnhancedFor(Int, "amt", Id("quantities"), [  Expr(FCall("deposit", [Id("myAccount"); Id("amt")]))]);
  EnhancedFor(Int, "amt", Id("quantities"), [  Expr(FCall("withdraw", [Id("myAccount"); Id("amt")]))]);
])]

(parse "function sampleProgram2() void {

  struct BankAccount { int balance; int ownerId; }
  BankAccount myAccount = new(BankAccount);
  myAccount.balance = 0; 
  myAccount.ownerId = 12345;

  array<int> quantities = [500,200,1400,3000,1000];

  function deposit(BankAccount act, int amount) int {
    act.balance = act.balance + amount;
    return act.balance;
  }

  function withdraw(BankAccount act, int amount) int {
    act.balance = act.balance - amount;
    return act.balance;
  }
  
  // add all the money
  for (int amt in quantities) { 
    deposit(myAccount, amt);
  }

  // take away all the money
  for (int amt in quantities) { 
    withdraw(myAccount, amt);
  }
}")

let full_prog_tests =
  "Full Programs" >:::
  [
    "Program 1" >:: prog_one_test;
    "Program 2" >:: prog_two_test;
  ]

let tests =
  "Parser" >:::
  [
    arithmetic_tests;
    logical_tests;
    common_tests;
    comment_tests;
    float_tests;
    vdec_tests;
    if_else_tests;
    for_tests;
    struct_tests;
    enhanced_for_tests;
    array_tests;
    new_tests;
    full_prog_tests;
  ]
