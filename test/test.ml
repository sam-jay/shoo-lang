open OUnit2

let suite =
  "suite">:::
  [
    TParser.tests;
  ];;

let () =
  run_test_tt_main suite;;