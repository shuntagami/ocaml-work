(* 目的：現在の気温t が15 以上25 以下かどうかをチェックする*)
(* kaiteki : int -> bool *)
let kaiteki t =
  15 <= t && t <= 25

(* 目的：現在の気温t から快適度を表す文字列を計算する*)
(* kion : int -> string *)
let kion t =
  if kaiteki t then "快適"
  else "普通"

(* テスト*)
let test1 = kion 7 = "普通"
let test2 = kion 15 = "快適"
let test3 = kion 20 = "快適"
let test4 = kion 25 = "快適"
let test5 = kion 28 = "普通"
