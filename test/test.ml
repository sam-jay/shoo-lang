open OUnit2

let suite =
  "suite">:::
  [
    TParser.tests;
    TFullPrograms.tests;
  ];;

let () =
  run_test_tt_main suite;;
