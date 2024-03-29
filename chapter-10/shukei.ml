(* 学生ひとり分のデータ（名前，点数，成績）を表す型*)
type gakusei_t = {
  namae : string; (* 名前*)
  tensuu : int; (* 点数*)
  seiseki : string; (* 成績*)
}

(* 目的: 学生リストlst のうち各成績の人数を集計する *)
(* shukei : gakusei_t list -> int * int * int * int *)
(* let rec shukei lst = match lst with *)
(*     [] -> (0, 0, 0, 0) *)
(*   | {namae = n; tensuu = t; seiseki = s} :: rest -> *)
(*     let shukei_rest = shukei rest in *)
(*     match shukei_rest with *)
(*       (a, b, c, d) -> if s = "A" then (a + 1, b, c, d) *)
(*       else if s = "B" then (a, b + 1, c, d) *)
(*       else if s = "C" then (a, b, c + 1, d) *)
(*       else (a, b, c, d + 1) *)
let rec shukei lst = match lst with
    [] -> (0, 0, 0, 0)
  | {namae = n; tensuu = t; seiseki = s} :: rest ->
    let (a, b, c, d) = shukei rest in (* 局所変数名のとこにパターンを書ける*)
    if s = "A" then (a + 1, b, c, d)
    else if s = "B" then (a, b + 1, c, d)
    else if s = "C" then (a, b, c + 1, d)
    else (a, b, c, d + 1)
