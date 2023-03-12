#use "../chapter-5/5.3.ml";;
#use "../chapter-8/8.3.ml";;

(* 目的: person_t型のデータのリストを受け取ったら，乙女座の人の名前のみからなるリストを返す *)
(* otomeza : person_t list -> string list *)
let rec otomeza lst = match lst with
    [] -> []
  | {name = n; tsuki = m; hi = d; ketsueki = k} :: rest ->
    if seiza m d = "おとめ" then n :: otomeza rest
    else otomeza rest

(* テスト *)
let test1 = otomeza [person1; person2; person3] = ["浅井"]
