(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2
open Batteries

type foo = { x : int; y : int }
module Db = Db.Make(struct
    type t = foo
    type id_t = int
    let id e = e.x
  end)

let test_save ctx =
  let db = Db.empty in
  assert_equal 0 (Db.size db);
  let db = db |> Db.save { x=10; y=0 } in
  assert_equal 1 (Db.size db);
  let db = db |> Db.save { x=20; y=1 } in
  assert_equal 2 (Db.size db)

let test_empty ctx =
  assert_equal 0 (Db.size Db.empty)

let test_id ctx =
  assert_equal 10 (Db.id { x=10; y=0 })

let test_delete ctx =
  let db1 = Db.empty
            |> Db.save {x=10; y=0} in
  assert_equal (Db.size db1) 1;
  let db2 = Db.delete 10 db1 in
  assert_equal (Db.size db2) 0

let test_delete_empty_db ctx =
  assert_equal
    0
    (Db.empty
     |> Db.save {x=10; y=0}
     |> Db.delete 10
     |> Db.delete 10
     |> Db.size)

let suite_db =
  "suite_db">:::
  ["test_save">:: test_save;
   "test_empty">:: test_empty;
   "test_id">:: test_id;
   "test_remove">:: test_delete;
   "test_remove_empty_db">:: test_delete_empty_db]

let () =
  print_endline "█ test_db.suite_db";
  run_test_tt_main suite_db
