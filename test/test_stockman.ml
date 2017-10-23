open OUnit2;;

let test_dumbest_ever ctx = assert_equal 1 1;;

let suite =
  "suite">:::
    ["test_dumbest_ever">:: test_dumbest_ever];;

let () =
  run_test_tt_main suite
