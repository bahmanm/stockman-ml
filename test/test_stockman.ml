open OUnit2;;

module R = Stockman.Products.Repo;;
module T = Stockman.Products;;
module E = Stockman.Products.Etl;;
module C = Stockman.Cmd;;
  
let test_product_db_add ctx =
  let db = R.empty in
  assert_equal 0 (R.size db);
  let db = db |> R.add new T.product in
  assert_equal 1 (R.size db);
  let db = db |> R.add new T.product in
  assert_equal 2 (R.size db);;
      
let test_product_db_empty ctx =
  assert_equal 0 (R.size R.empty);;

let suite_product =
  "suite_product">:::
  ["test_product_db_add">:: test_product_db_add;
   "test_product_db_empty">:: test_product_db_empty];;

(******************************************************************************)
let test_etl_product_of_string_valid_input ctx =
  let () = match E.product_of_string ',' "p1,10" with
    | Ok p -> assert_equal "p1" p#get_name;
      assert_equal 10 p#get_qty;
    | _ -> assert_equal 0 1 in
  let () = match E.product_of_string ';' "p1 ;10 " with
    | Ok p -> assert_equal "p1" p#get_name;
      assert_equal 10 p#get_qty;
    | _ -> assert_equal 0 1 in
  match E.product_of_string '-' "   p1 -  10 " with
  | Ok p -> assert_equal "p1" p#get_name;
      assert_equal 10 p#get_qty
  | _ -> assert_equal 0 1;;

let test_etl_product_of_string_invalid_input ctx =
  let () = match E.product_of_string ',' ",10" with
    | Error msg -> assert_equal msg "Invalid row: ,10"
    | _ -> assert_equal 0 1 in
  let () = match E.product_of_string ';' "p1, " with
    | Error msg -> assert_equal msg "Invalid row: p1, "
    | _ -> assert_equal 0 1 in
  let () = match E.product_of_string ',' "p1" with
    | Error msg -> assert_equal msg "Invalid row: p1"
    | _ -> assert_equal 0 1 in
  match E.product_of_string '-' "" with
  | Error msg -> assert_equal msg "Empty row"
  | _ -> assert_equal 0 1;;

let test_etl_repo_of_file_valid_input ctx =
  let repo = E.repo_of_file "res/wp1-products__all-valid.csv" "#" ',' 1 in
  assert_equal 4 (R.size repo);;

let test_etl_repo_of_file_invalid_input ctx =
  let repo1 = E.repo_of_file "res/wp1-products__lots-of-invalid-rows.csv" "#" ';' 1 in
  assert_equal 1 (R.size repo1);
  let repo2 = E.repo_of_file "res/wp1-products__lots-of-invalid-rows.csv" "#" '~' 1 in
  assert_equal true (R.is_empty repo2);;

let suite_etl =
  "suite_etl">:::
  ["test_etl_product_of_string_valid_input">:: test_etl_product_of_string_valid_input;
   "test_etl_product_of_string_invalid_input">:: test_etl_product_of_string_invalid_input;
   "test_etl_repo_of_file_valid_input">:: test_etl_repo_of_file_valid_input;
   "test_etl_repo_of_file_invalid_input">:: test_etl_repo_of_file_invalid_input];;

(******************************************************************************)
let test_cmd_all_options_present ctx =
  let argv = [|"app_name"; "-f"; "/home/bahman/t.tmp"; "-d"; ";"|] in
  let opts = C.parse argv in
  match opts.C.file_path with
  | Some s -> assert_equal s "/home/bahman/t.tmp"
  | None -> ignore (assert_failure "shouldn't happen");
  match opts.C.field_delim with
  | Some c -> assert_equal c ';'
  | None -> assert_failure "shouldn't happen";;

let test_cmd_only_file_path_options_present ctx =
  let argv = [|"app_name"; "-f"; "/home/bahman/t.tmp"|] in
  let opts = C.parse argv in
  match opts.C.file_path with
  | Some s -> assert_equal s "/home/bahman/t.tmp"
  | None -> ignore (assert_failure "shouldn't happen");
  match opts.C.field_delim with
  | Some c -> assert_equal c ','
  | None -> assert_failure "shouldn't happen";;

let suite_cmd =
  "suite_cmd">:::
  ["test_cmd_all_options_present">:: test_cmd_all_options_present;
   "test_cmd_only_file_path_options_present">:: test_cmd_only_file_path_options_present];;

let () =
  run_test_tt_main suite_product;
  run_test_tt_main suite_etl;
  run_test_tt_main suite_cmd;;
