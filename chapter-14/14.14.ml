(* 目的：リスト中の文字列をつなげた文字列を返す *)
(* concat : string list -> string *)
let concat lst = List.fold_right (^) lst "" (* infix関数の^を()で囲むことでprefix関数にする*)

(* テスト *)
let test1 = concat [] = ""
let test2 = concat ["あ"] = "あ"
let test3 = concat ["春"; "夏"; "秋"; "冬"] = "春夏秋冬"
