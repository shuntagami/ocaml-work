(* 目的: 名前と成績の組を受け取ったら「OOさんの評価は△です」という文字列を返す *)
(* seiseki : string * string -> string *)
let seiseki pair = match pair with
    (name, score) -> name ^ "さんの成績は" ^ score ^ "です"

(* テスト *)
let test1 = seiseki ("shun", "A") = "shunさんの成績はAです"
