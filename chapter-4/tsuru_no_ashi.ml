(* 目的: 鶴の数xに応じた足の本数を計算する *)
(* tsuru_no_ashi : int -> int *)
let tsuru_no_ashi x = x * 2

(* テスト *)
let test1 = tsuru_no_ashi 1 = 2
let test2 = tsuru_no_ashi 2 = 4
let test3 = tsuru_no_ashi 10 = 20

(* 目的: 亀の数xに応じた足の本数を計算する *)
(* kame_no_ashi : int -> int *)
let kame_no_ashi x = x * 4

(* テスト *)
let test4 = kame_no_ashi 1 = 4
let test5 = kame_no_ashi 2 = 8
let test6 = kame_no_ashi 10 = 40

(* 目的: 鶴の数xと亀の数yに応じた足の本数を計算する *)
(* tsurukame_no_ashi : int -> int -> int *)
let tsurukame_no_ashi x y = tsuru_no_ashi x + kame_no_ashi y

(* テスト *)
let test7 = tsurukame_no_ashi 1 1 = 6
let test8 = tsurukame_no_ashi 2 3 = 16
let test9 = tsurukame_no_ashi 3 5 = 26

(* 目的: 鶴と亀の合計count_tsuru_kameと足の数count_legを与えられたら鶴の数count_tsuruを計算する *)
(* tsrukame : int -> int -> int *)
let tsurukame count_tsuru_kame count_leg =
  (tsurukame_no_ashi 0 count_tsuru_kame - count_leg)/2

(* テスト *)
let test10 = tsurukame 2 6 = 1
let test11 = tsurukame 10 32 = 4
let test12 = tsurukame 100 274 = 63
