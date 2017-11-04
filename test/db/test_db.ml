open OUnit2
open Batteries

type foo = { x : int; y : int }
module Db = Db.Make(struct type t = foo end)

let test_db_add ctx =
  let db = Db.empty in
  assert_equal 0 (Db.size db);
  let db = db |> Db.add { x=10; y=0 } in
  assert_equal 1 (Db.size db);
  let db = db |> Db.add { x=20; y=1 } in
  assert_equal 2 (Db.size db)

let test_db_empty ctx =
  assert_equal 0 (Db.size Db.empty)

let suite_db =
  "suite_db">:::
  ["test_db_add">:: test_db_add;
   "test_db_empty">:: test_db_empty]

let () =
  print_endline "test_db.suite_db";
  run_test_tt_main suite_db;
