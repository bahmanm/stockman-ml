(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2
open Batteries

let test_validate_no_lines ctx =
  assert_equal 0 1

let test_validate_lines_ok ctx =
  assert_equal 0 1

let test_validate_invalid_lines ctx =
  assert_equal 0 1

let test_validate_invalid_header ctx =
  assert_equal 0 1

let test_validate_all_ok ctx =
  assert_equal 0 1

let suite_invoice =
  "suite_invoice">:::
  ["test_validate_no_lines">:: test_validate_no_lines;
   "test_validate_lines_ok">:: test_validate_lines_ok;
   "test_validate_invalid_lines">:: test_validate_invalid_lines;
   "test_validate_invalid_header">:: test_validate_invalid_header;
   "test_validate_all_ok">:: test_validate_all_ok]
