(* 目的: 誕生日（m, d）を受け取ったら星座を返す *)
(* seiza : int -> int -> string *)
(* let seiza m d = *)
(*   if m = 1 then (if d <= 19 then "やぎ" else "みずがめ") *)
(*   else if m = 2 then (if d <= 18 then "みずがめ" else "うお") *)
(*   else if m = 3 then (if d <= 20 then "うお" else "おひつじ") *)
(*   else if m = 4 then (if d <= 19 then "おひつじ" else "おうし") *)
(*   else if m = 5 then (if d <= 20 then "おうし" else "ふたご") *)
(*   else if m = 6 then (if d <= 21 then "ふたご" else "かに") *)
(*   else if m = 7 then (if d <= 22 then "かに" else "しし") *)
(*   else if m = 8 then (if d <= 22 then "しし" else "おとめ") *)
(*   else if m = 9 then (if d <= 22 then "おとめ" else "てんびん") *)
(*   else if m = 10 then (if d <= 23 then "てんびん" else "さそり") *)
(*   else if m = 11 then (if d <= 22 then "さそり" else "いて") *)
(*   else if m = 12 then (if d <= 21 then "いて" else "やぎ") *)
(*   else "" *)

let seiza m d =
  match m with
  | 1 -> if d <= 19 then "やぎ" else "みずがめ"
  | 2 -> if d <= 18 then "みずがめ" else "うお"
  | 3 -> if d <= 20 then "うお" else "おひつじ"
  | 4 -> if d <= 19 then "おひつじ" else "おうし"
  | 5 -> if d <= 20 then "おうし" else "ふたご"
  | 6 -> if d <= 21 then "ふたご" else "かに"
  | 7 -> if d <= 22 then "かに" else "しし"
  | 8 -> if d <= 22 then "しし" else "おとめ"
  | 9 -> if d <= 22 then "おとめ" else "てんびん"
  | 10 -> if d <= 23 then "てんびん" else "さそり"
  | 11 -> if d <= 22 then "さそり" else "いて"
  | 12 -> if d <= 21 then "いて" else "やぎ"
  | _ -> ""

(* テスト *)
let test1 = seiza 1 1 = "やぎ"
let test2 = seiza 1 19 = "やぎ"
let test3 = seiza 1 20 = "みずがめ"
let test4 = seiza 2 18 = "みずがめ"
let test5 = seiza 2 19 = "うお"
let test6 = seiza 3 20 = "うお"
let test7 = seiza 3 21 = "おひつじ"
let test8 = seiza 4 19 = "おひつじ"
let test9 = seiza 4 20 = "おうし"
let test10 = seiza 5 20 = "おうし"
let test11 = seiza 5 21 = "ふたご"
let test12 = seiza 6 21 = "ふたご"
let test13 = seiza 6 22 = "かに"
let test14 = seiza 7 22 = "かに"
let test15 = seiza 7 23 = "しし"
let test16 = seiza 8 22 = "しし"
let test17 = seiza 8 23 = "おとめ"
let test18 = seiza 9 22 = "おとめ"
let test19 = seiza 9 23 = "てんびん"
let test20 = seiza 10 23 = "てんびん"
let test21 = seiza 10 24 = "さそり"
let test22 = seiza 11 22 = "さそり"
let test23 = seiza 11 23 = "いて"
let test24 = seiza

