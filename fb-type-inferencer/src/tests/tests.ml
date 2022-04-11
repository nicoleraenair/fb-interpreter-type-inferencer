open Efb;;
open Fb;;
open OUnit2;;

open Efbinference;;
open Efbtype;;
open Fbast;;
open Fbinterp;;

let fourTest = "4 => 4" >:: fun _ ->
  let expected = Int 4 in
  let source = Int 4 in
  let actual = eval source in
  assert_equal ~printer:show_expr expected actual

let tests = [
];;

run_test_tt_main ("all tests" >::: tests);;
