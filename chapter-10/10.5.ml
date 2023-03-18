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
let lst1 = [gakusei2]
let lst2 = [gakusei3; gakusei4]
let lst3 = [gakusei4; gakusei3]
let lst4 = [gakusei4; gakusei1; gakusei6; gakusei5; gakusei2; gakusei3]

(* 学生のデータの例 *)
let gakusei1 = {namae="nakamura"; tensuu=90; seiseki="A"}
let gakusei2 = {namae="miyahara"; tensuu=80; seiseki="A"}
let gakusei3 = {namae="sato"; tensuu=75; seiseki="B"}
let gakusei4 = {namae="idehara"; tensuu=70; seiseki="B"}
let gakusei5 = {namae="tsubata"; tensuu=65; seiseki="C"}
let gakusei6 = {namae="asai"; tensuu=60; seiseki="C"}

(* 目的: gakusei_t型のリストを受け取ったらその中から最高得点を受け取った人のレコードを返す *)
(* gakusei_max : gakusei_t list -> gakusei_t *)
let rec gakusei_max lst = match lst with
    [] -> {namae = ""; tensuu = min_int; seiseki = ""}
  | ({namae = n; tensuu = t; seiseki = s} as gakusei) :: rest ->
    let rest_max = gakusei_max rest in
    match rest_max with {namae = n_max; tensuu = t_max; seiseki = s_max} ->
      if t >= t_max then gakusei
      else rest_max

(* テスト *)
let test1 = gakusei_max lst1 = gakusei2
let test2 = gakusei_max lst2 = gakusei3
let test3 = gakusei_max lst3 = gakusei3
let test4 = gakusei_max lst4 = gakusei1
