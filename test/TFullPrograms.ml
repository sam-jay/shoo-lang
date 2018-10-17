open OUnit2
open Ast

let parse input =
  let lexbuf = Lexing.from_string input in
  Parser.program Scanner.token lexbuf

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
    ], false);
    FDecl("foldl", [(Func, "f"); (Int, "acc"); (Array(Int), "items")], Array(Int), [
      If(Binop(FCall("length", [Id("items")]), Equal, IntLit(0)),
        [Return(Id("acc"))],
        [Return(FCall("foldl", [Id("f"); FCall("f", [Id("acc"); FCall("first", [Id("items")])]); FCall("rest", [Id("items")])]))])
    ], false);
    FDecl("map", [(Func, "f"); (Array(Int), "items")], Array(Int), [
      If(Binop(FCall("length", [Id("items")]), Equal, IntLit(0)),
        [Return(ArrayLit([]))],
        [Return(FCall("concat",[
          FCall("f", [FCall("first", [Id("items")])]);
          FCall("map", [Id("f"); FCall("rest", [Id("items")])])]))])
    ], false);
    VDecl(Array(Int), "results", Some(FCall("map", [
      FExpr([(Array(Int), "task")],
        Array(Int),
        [Return(FCall("foldl", [Id("sum"); IntLit(0); Id("task")]))]);
      Id("tasks")
    ])));
    Expr(FCall("print", [FCall("stringOfInt", [FCall("foldl", [Id("sum"); IntLit(0); Id("results")])])]));
  ], false)]
  (parse "function sampleProgram1() void {
    array<array<int>> tasks = [
      [1,2,3,4,5,6,7,8,9,10],
      [11,12,13,14,15,16,17,18,19,20]
    ];

    function sum(int x, int y) int {
      return x + y;
    }

    function foldl(func f, int acc, array<int> items) array<int> {
      if (length(items) == 0) {
        return acc;
      } else {
        return foldl(f, f(acc, first(items)), rest(items));
      }
    }

    function map(func f, array<int> items) array<int> {
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
  VDecl(Struct("BankAccount"), "aliceAccount", Some(New(NStruct("BankAccount"))));
  Expr(Assign(Dot(Id("aliceAccount"), "balance"), IntLit(0)));
  Expr(Assign(Dot(Id("aliceAccount"), "ownerId"), IntLit(12345)));

  VDecl(Struct("BankAccount"), "bobAccount", Some(New(NStruct("BankAccount"))));
  Expr(Assign(Dot(Id("bobAccount"), "balance"), IntLit(0)));
  Expr(Assign(Dot(Id("bobAccount"), "ownerId"), IntLit(12346)));

  VDecl(Array(Int), "quantities", Some(ArrayLit([
      IntLit(500); IntLit(200); IntLit(1400); IntLit(3000); IntLit(1000);
  ])));
  VDecl(Array(Bool), "coinTossHeadsDeposit", Some(ArrayLit([
    BoolLit(true); BoolLit(true); BoolLit(false); BoolLit(true); BoolLit(false);
  ])));
  VDecl(Array(Bool), "coinTossHeadsWithdraw", Some(ArrayLit([
    BoolLit(false); BoolLit(true); BoolLit(true); BoolLit(false); BoolLit(false);
  ])));
  FDecl("deposit", [(Struct("BankAccount"), "act"); (Int, "amount")], Int, [
    Expr(Assign(Dot(Id("act"), "balance"), Binop(Dot(Id("act"), "balance"), Add, Id("amount"))));
    Return(Dot(Id("act"), "balance"));
  ], false);
  FDecl("withdraw", [(Struct("BankAccount"), "act"); (Int, "amount")], Int, [
    Expr(Assign(Dot(Id("act"), "balance"), Binop(Dot(Id("act"), "balance"), Sub, Id("amount"))));
    Return(Dot(Id("act"), "balance"));
  ], false);
  EnhancedFor(Bool, "isAlice", Id("coinTossHeadsDeposit"), 
    [If(Id("isAlice"),[EnhancedFor(Int, "amt", Id("quantities"), [  Expr(FCall("deposit", [Id("aliceAccount"); Id("amt")]))])],
    [EnhancedFor(Int, "amt", Id("quantities"), [  Expr(FCall("deposit", [Id("bobAccount"); Id("amt")]))])])]
  );
  EnhancedFor(Bool, "isBob", Id("coinTossHeadsWithdraw"), 
    [If(Id("isBob"),[EnhancedFor(Int, "amt", Id("quantities"), [  Expr(FCall("withdraw", [Id("bobAccount"); Id("amt")]))])],
    [EnhancedFor(Int, "amt", Id("quantities"), [  Expr(FCall("withdraw", [Id("aliceAccount"); Id("amt")]))])])]
  );
  VDecl(Int, "i", None);
  ForLoop(Some(Expr(Assign(Id("i"), IntLit(0)))), Some(Binop(Id("i"), Less, IntLit(2))), 
    Some(Assign(Id("i"), Binop(Id("i"), Add, IntLit(1)))), 
    [If(Binop(Id("i"), Equal, IntLit(0)),
    [Expr(FCall("print", [FCall("stringOfInt", [Dot(Id("aliceAccount"), "balance")])]));],
    [Expr(FCall("print", [FCall("stringOfInt", [Dot(Id("bobAccount"), "balance")])]));])]);
], false)]


(parse "function sampleProgram2() void {

  struct BankAccount { int balance; int ownerId; }

  BankAccount aliceAccount = new(BankAccount);
  aliceAccount.balance = 0; 
  aliceAccount.ownerId = 12345;

  BankAccount bobAccount = new(BankAccount);
  bobAccount.balance = 0; 
  bobAccount.ownerId = 12346;

  array<int> quantities = [500,200,1400,3000,1000];

  /* for depositing heads will be aliceAccount and tails will be bobAccount */
  array<bool> coinTossHeadsDeposit = [true,true,false,true,false];

  /* for withdrawal heads will be bobAccount and tails will be aliceAccount */
  array<bool> coinTossHeadsWithdraw = [false,true,true,false,false];

  function deposit(BankAccount act, int amount) int {
    act.balance = act.balance + amount;
    return act.balance;
  }

  function withdraw(BankAccount act, int amount) int {
    act.balance = act.balance - amount;
    return act.balance;
  }
  
  // add all the quantities to each person per coin flip
  for (bool isAlice in coinTossHeadsDeposit) {
    if (isAlice) {
      for (int amt in quantities) { 
        deposit(aliceAccount, amt);
      }    
    } else {
      for (int amt in quantities) { 
        deposit(bobAccount, amt);
      }    
    }
  }

  // add all the quantities to each person per coin flip
  for (bool isBob in coinTossHeadsWithdraw) {
    if (isBob) {
      for (int amt in quantities) { 
        withdraw(bobAccount, amt);
      }    
    } else {
      for (int amt in quantities) { 
        withdraw(aliceAccount, amt);
      }    
    }
  }
  int i;

  // a trivial for loop to print results
  for (i = 0; i<2 ; i=i+1) {
    if (i == 0) {
      print(stringOfInt(aliceAccount.balance));
    } else {
      print(stringOfInt(bobAccount.balance));
    }
  }
}")

let tests =
  "Full Programs" >:::
  [
    "Program 1" >:: prog_one_test;
    "Program 2" >:: prog_two_test;
  ]
