open OUnit2
open Batteries

type foo = { x : int; y : int }
module Db = Db.Make(struct
    type t = foo
    type id_t = int
    let id e = e.x
  end)

let test_add ctx =
  let db = Db.empty in
  assert_equal 0 (Db.size db);
  let db = db |> Db.add { x=10; y=0 } in
  assert_equal 1 (Db.size db);
  let db = db |> Db.add { x=20; y=1 } in
  assert_equal 2 (Db.size db)

let test_empty ctx =
  assert_equal 0 (Db.size Db.empty)

let test_id ctx =
  assert_equal 10 (Db.id { x=10; y=0 })

let suite_db =
  "suite_db">:::
  ["test_add">:: test_add;
   "test_empty">:: test_empty;
   "test_id">:: test_id]

let () =
  print_endline "test_db.suite_db";
  run_test_tt_main suite_db;
