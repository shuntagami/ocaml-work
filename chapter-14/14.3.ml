(* 目的：ふたつの文字列をつなげる *)
(* append_string : string -> string -> string *)
let append_string first rest_result = first ^ rest_result

(* 目的: 受け取ったlstの要素を前から順に全部くっつけた文字列を返す *)
(* concat : string list -> string *)
let concat lst = List.fold_right append_string lst ""

(* テスト *)
let test1 = concat [] = ""
let test2 = concat ["あ"] = "あ"
let test3 = concat ["春"; "夏"; "秋"; "冬"] = "春夏秋冬"
