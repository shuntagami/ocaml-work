(* 学生ひとり分のデータ（名前，点数，成績）を表す型*)
type gakusei_t = {
  namae : string; (* 名前*)
  tensuu : int; (* 点数*)
  seiseki : string; (* 成績*)
}

(* 学生のデータの例 *)
let gakusei1 = {namae="nakamura"; tensuu=90; seiseki="A"}
let gakusei2 = {namae="miyahara"; tensuu=80; seiseki="A"}
let gakusei3 = {namae="sato"; tensuu=75; seiseki="B"}
let gakusei4 = {namae="idehara"; tensuu=70; seiseki="B"}
let gakusei5 = {namae="tsubata"; tensuu=65; seiseki="C"}
let gakusei6 = {namae="asai"; tensuu=60; seiseki="C"}

(* 学生のリストの例 *)
let lst1 = []
let lst2 = [gakusei2]
let lst3 = [gakusei3; gakusei4]
let lst4 = [gakusei4; gakusei3]
let lst5 = [gakusei4; gakusei1; gakusei6; gakusei5; gakusei2; gakusei3]

(* 目的：昇順に並んでいる lst の正しい位置に gakusei を挿入する *)
(* gakusei_insert : gakusei_t list -> gakusei_t -> gakusei_t list *)
let rec gakusei_insert lst gakusei0 = match lst with
    [] -> [gakusei0]
  | ({namae = n; tensuu = t; seiseki = s;} as gakusei) :: rest ->
    match gakusei0 with {namae = n0; tensuu = t0; seiseki = s0} ->
      if t0 <= t then gakusei0 :: lst
      else gakusei :: gakusei_insert rest gakusei0

(* テスト *)
let test1 = gakusei_insert [] gakusei2 = [gakusei2]
let test2 = gakusei_insert [gakusei4] gakusei3 = [gakusei4; gakusei3]
let test3 = gakusei_insert [gakusei3] gakusei4 = [gakusei4; gakusei3]
let test4 = gakusei_insert [gakusei6; gakusei5; gakusei3; gakusei2; gakusei1] gakusei4
            = [gakusei6; gakusei5; gakusei4; gakusei3; gakusei2; gakusei1]

(* 目的: gakusei_t型のリストを受け取ったらtensuuフィールドの順に整列 *)
(* gakusei_ins_sort : gakusei_t list -> gakusei_t list *)
let rec gakusei_ins_sort lst = match lst with
    [] -> []
  | first :: rest -> gakusei_insert (gakusei_ins_sort rest) first

(* テスト *)
let test5 = gakusei_ins_sort lst1 = []
let test6 = gakusei_ins_sort lst2 = [gakusei2]
let test7 = gakusei_ins_sort lst3 = [gakusei4; gakusei3]
let test8 = gakusei_ins_sort lst4 = [gakusei4; gakusei3]
let test9 = gakusei_ins_sort lst5
            = [gakusei6; gakusei5; gakusei4; gakusei3; gakusei2; gakusei1]
