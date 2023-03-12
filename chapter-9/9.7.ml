#use "../chapter-8/8.3.ml";;

(* 目的: person_t型のデータのリストを受け取ったら血液型A型の人の数を返す *)
(* count_ketsueki_A : person_t list -> int *)
let rec count_ketsueki_A lst = match lst with
    [] -> 0
  | {ketsueki = k} :: rest
    -> if k = "A" then 1 + count_ketsueki_A rest
    else count_ketsueki_A rest

(* テスト *)
let test1 = count_ketsueki_A [person1; person2; person3] = 1
