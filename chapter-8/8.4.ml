#use "./8.3.ml"

(* 目的: 人のデータperson_tを受け取り「OOさんの血液型はXX型です」という文字列を返す *)
(* ketsueki_hyoji : person_t -> string *)
let ketsueki_hyoji person_t = match person_t with
    {name = n; ketsueki = k;} ->
      n ^ "さんの血液型は" ^ k ^ "型です"

(* テスト *)
let test1 = ketsueki_hyoji person1 = "浅井さんの血液型はA型です"
let test2 = ketsueki_hyoji person2 = "宮原さんの血液型はB型です"
let test3 = ketsueki_hyoji person3 = "中村さんの血液型はO型です"
