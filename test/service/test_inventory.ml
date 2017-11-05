(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2
open Batteries

module Db = StkDomainDb.ProductDb
module P = StkDomain.Product
module S = StkService.Inventory

let test_stock_in_existing_item ctx =
  match
    Db.empty
    |> Db.save { P.name="p1"; P.qty=10; P.amt=100.00 }
    |> S.stock_in { P.name="p1"; P.qty=2; P.amt=50.00 }
  with
  | Ok db -> (
    match Db.get "p1" db with
    | Some p ->
      assert_equal p.P.qty 12;
      assert_equal p.P.amt 150.00
    | None -> assert_failure "shouldn't happen"
  )
  | Bad _ -> assert_failure "shouldn't happen"
  
let suite_inventory =
  "suite_inventory">:::
  ["test_stock_in_existing_item">:: test_stock_in_existing_item]
