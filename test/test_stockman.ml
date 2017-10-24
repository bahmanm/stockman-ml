open OUnit2;;

module R = Stockman.Products.Repo;;
module T = Stockman.Products;;
  
let test_product_db_add ctx =
  let db = R.empty in
  assert_equal 0 (R.size db);
  let db = db |> R.add new T.product in
  assert_equal 1 (R.size db);
  let db = db |> R.add new T.product in
  assert_equal 2 (R.size db);;
      
let test_product_db_empty ctx =
  assert_equal 1 (R.size R.empty);;

let suite =
  "suite">:::
    ["test_product_db_add">:: test_product_db_add;
     "test_product_db_empty">:: test_product_db_empty];;

let () =
  run_test_tt_main suite
