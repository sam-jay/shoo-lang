open OUnit2

let suite =
  "suite">:::
  [
    (*TParser.tests;*)
    TFullPrograms.tests;
    TSemant.tests;
  ];;

let () =
  run_test_tt_main suite;;
