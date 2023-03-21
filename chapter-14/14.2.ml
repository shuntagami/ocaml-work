(* 学生ひとり分のデータ（名前，点数，成績）を表す型*)
type gakusei_t = {
  namae : string; (* 名前*)
  tensuu : int; (* 点数*)
  seiseki : string; (* 成績*)
}

(* gakusei_t list 型のデータの例*)
let lst1 = []
let lst2 = [{namae = "asai"; tensuu = 70; seiseki = "B"}]
let lst3 = [{namae = "asai"; tensuu = 70; seiseki = "B"};
            {namae = "kaneko"; tensuu = 85; seiseki = "A"}]
let lst4 = [{namae = "yoshida"; tensuu = 80; seiseki = "A"};
            {namae = "asai"; tensuu = 70; seiseki = "B"};
            {namae = "kaneko"; tensuu = 85; seiseki = "A"}]

(* 目的：リストlst の中から条件p を満たす要素のみを取り出す*)
(* filter : (’a -> bool) -> ’a list -> ’a list *)
let rec filter p lst = match lst with
    [] -> []
  | first :: rest -> if p first then first :: filter p rest
    else filter p rest

(* 目的：学生リストlst のうち成績がA の人の数を返す*)
(* count_A : gakusei_t list -> int *)
let count_A list =
  let is_A gakusei =
    match gakusei with {namae = n; tensuu = t; seiseki = s} ->
      s = "A" in
  List.length(filter is_A list)

(* テスト *)
let test1 = count_A lst1 = 0
let test2 = count_A lst2 = 0
let test3 = count_A lst3 = 1
let test4 = count_A lst4 = 2
