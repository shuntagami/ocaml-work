(* 目的：実数のリストlst を受け取り各要素の平方根のリストを返す*)
(* map_sqrt : float list -> float list *)
let rec map_sqrt lst = match lst with
    [] -> []
  | first :: rest -> sqrt first :: map_sqrt rest

(* 学生ひとり分のデータ（名前，点数，成績）を表す型*)
type gakusei_t = {
  namae : string; (* 名前*)
  tensuu : int; (* 点数*)
  seiseki : string; (* 成績*)
}

(* 目的：学生のデータgakusei を受け取り成績のついたデータを返す*)
(* hyouka : gakusei_t -> gakusei_t *)
let hyouka gakusei = match gakusei with
    {namae = n; tensuu = t; seiseki = s} ->
    if t >= 80 then {namae = n; tensuu = t; seiseki = "A"}
    else if t >= 70 then {namae = n; tensuu = t; seiseki = "B"}
    else if t >= 60 then {namae = n; tensuu = t; seiseki = "C"}
    else {namae = n; tensuu = t; seiseki = "D"}

(* 目的：学生のリストlst を受け取り成績を入れたリストを返す*)
(* map_hyouka : gakusei_t list -> gakusei_t list *)
let rec map_hyouka lst = match lst with
    [] -> []
  | first :: rest -> hyouka first :: map_hyouka rest

(* 目的：関数f とリストlst を受け取りf を施したリストを返す、一般化*)
(* map : (’a -> ’b) -> ’a list -> ’b list *)
let rec map f lst = match lst with
    [] -> []
  | first :: rest -> f first :: map f rest

(* 目的：実数のリストlst を受け取り各要素の平方根のリストを返す*)
(* map_sqrt : float list -> float list *)
let map_sqrt lst = map sqrt lst

(* 目的：学生のリストlst を受け取り成績を入れたリストを返す*)
(* map_hyouka : gakusei_t list -> gakusei_t list *)
let map_hyouka lst = map hyouka lst
