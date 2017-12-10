(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2
open Batteries

let name = "test_db."
type foo = { x : int; y : int }
module Db = Db.Make(struct
    type t = foo
    type id_t = int
    let id e = e.x
    let validate e =
      if e.x >= 0 then Ok e else Bad "x"
  end)

(**********************************************************)
module Save = struct
  let name = name ^ "save"

  (************************)
  let test_valid_item ctx =
    let db = Db.empty in
    assert_equal 0 (Db.size db);
    let db = db |> Db.save { x=10; y=0 } in
    assert_equal 1 (Db.size db);
    let db = db |> Db.save { x=20; y=1 } in
    assert_equal 2 (Db.size db)

  (************************)
  let test_invalid_item ctx =
    try
      let _ = Db.empty |> Db.save { x = -1; y=10 } in
      assert_failure "shouldn't happen"
    with Db.InvalidField (e, f) -> assert true

  (************************)
  let suite =
    "save" >:::
    ["valid items"
     >:: test_valid_item;
     "invalid items shoudl raise exception"
     >:: test_invalid_item]
end

(**********************************************************)
module Empty = struct
  let name = name ^ "empty"

  (************************)
  let test_size ctx =
    assert_equal 0 (Db.size Db.empty)

  (************************)
  let suite =
    name >:::
    ["empty db should have size 0"
     >:: test_size]
end

(**********************************************************)
module Delete = struct
  let name = name ^ "delete"

  (************************)
  let test_only_item_in_db ctx =
    let db1 = Db.empty
              |> Db.save {x=10; y=0} in
    assert_equal (Db.size db1) 1;
    let db2 = Db.delete 10 db1 in
    assert_equal (Db.size db2) 0

  (************************)
  let test_empty_db ctx =
    assert_equal
      0
      (Db.empty
       |> Db.save {x=10; y=0}
       |> Db.delete 10
       |> Db.delete 10
       |> Db.size)

  (************************)
  let suite =
    name >:::
    ["db size should be 0 after deleting the only item"
     >:: test_only_item_in_db;
     "deleting on an already empty database has no effect"
     >:: test_empty_db]
end

(**********************************************************)
module Contains = struct
  let name = name ^ "contains"

  (************************)
  let test_nonempty_db ctx =
    let db1 =
      Db.empty
      |> Db.save {x=10; y=0}
      |> Db.save {x=8; y=2}
      |> Db.save {x=6; y=4} in
    assert_equal true (Db.contains 8 db1);
    assert_equal false (Db.contains 20 db1)

  (************************)
  let test_empty_db ctx =
    assert_equal false (Db.empty |> Db.contains 10)

  (************************)
  let suite =
    name >:::
    ["contains on non-empty db"
     >:: test_nonempty_db;
     "contains on empty db"
     >:: test_empty_db]
end

(**********************************************************)
module Get = struct
  let name = name ^ "get"

  (************************)
  let test_nonempty_db_found ctx =
    let db1 =
      Db.empty
      |> Db.save {x=10; y=0}
      |> Db.save {x=8; y=2}
      |> Db.save {x=6; y=4} in
    assert_equal (Db.get 10 db1) (Some {x=10; y=0});
    assert_equal (Db.get 6 db1) (Some {x=6; y=4});
    assert_equal (Db.get 8 db1) (Some {x=8; y=2})

  (************************)
  let test_empty_db ctx =
    assert_equal None (Db.get 10 Db.empty)

  (************************)
  let test_nonempty_db_notfound ctx =
    let db1 =
      Db.empty
      |> Db.save {x=10; y=0}
      |> Db.save {x=8; y=2}
      |> Db.save {x=6; y=4} in
    assert_equal None (Db.get 12 db1);
    assert_equal None (Db.get 0 db1)

  (************************)
  let suite =
    name >:::
    ["get existing items should return the item"
     >:: test_nonempty_db_found;
     "get non-existing itesm should return none"
     >:: test_nonempty_db_notfound;
     "get on empty db should return none"
     >:: test_empty_db]
end

(**********************************************************)
let print_name s =
  print_endline ("\n»» test_db." ^ s)

let () =
  print_name Save.name;
  run_test_tt_main Save.suite;

  print_name Empty.name;
  run_test_tt_main Empty.suite;

  print_name Delete.name;
  run_test_tt_main Delete.suite;

  print_name Contains.name;
  run_test_tt_main Contains.suite;

  print_name Get.name;
  run_test_tt_main Get.suite
