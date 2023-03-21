(* 目的：受け取ったリストlst の各要素の和を求める*)
(* sum : int list -> int *)
let rec sum lst = match lst with
    [] -> 0
  | first :: rest -> first + sum rest

(* 目的：受け取ったリストlst の長さを求める*)
(* length : ’a list -> int *)
let rec length lst = match lst with
    [] -> 0
  | first :: rest -> 1 + length rest

(* 目的：lst1 とlst2 を受け取りそれらを結合したリストを返す*)
(* append : ’a list -> ’a list -> ’a list *)
let rec append lst1 lst2 = match lst1 with
    [] -> lst2
  | first :: rest -> first :: append rest lst2

(* 目的：init から始めてlst の要素を右から順にf を施し込む*)
(* fold_right : (’a -> ’b -> ’b) -> ’a list -> ’b -> ’b *)
(* fold_right f [1; 2; 3; 4; 5] init は
f 1 (f 2 (f 3 (f 4 (f 5 init))))
の意味 *)
let rec fold_right f lst init = match lst with
    [] -> init
  | first :: rest -> f first (fold_right f rest init)

(* 目的：受け取ったリストlst の各要素の和を求める*)
(* sum : int list -> int *)
let sum lst =
  fold_right (fun first rest_result -> first + rest_result)
    lst 0

(* 目的：受け取ったリストlst の長さを求める*)
(* length : ’a list -> int *)
let length lst =
  fold_right (fun first rest_result -> 1 + rest_result) lst 0

(* 目的：lst1 とlst2 を受け取りそれらを結合したリストを返す*)
(* append : ’a list -> ’a list -> ’a list *)
let append lst1 lst2 =
  fold_right (fun first rest_result -> first :: rest_result)
    lst1 lst2
