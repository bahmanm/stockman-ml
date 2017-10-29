open OUnit2
open Batteries

module D = Product.Db
module P = Product

let test_product_db_add ctx =
  let db = D.empty in
  assert_equal 0 (D.size db);
  let db = db |> D.add { P.name="foo"; P.qty=1 } in
  assert_equal 1 (D.size db);
  let db = db |> D.add { P.name="bar"; P.qty=1 } in
  assert_equal 2 (D.size db)

let test_product_db_empty ctx =
  assert_equal 0 (D.size D.empty)

let suite_product =
  "suite_product">:::
  ["test_product_db_add">:: test_product_db_add;
   "test_product_db_empty">:: test_product_db_empty]
