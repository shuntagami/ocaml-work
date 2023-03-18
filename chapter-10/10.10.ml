#use "../chapter-9/9.9.ml"

(* 目的: ローマ字の駅名と駅名リストを受け取ったらその駅の漢字表記を返す *)
(* romaji_to_kanji : string -> ekimei_t list -> string *)
let rec romaji_to_kanji romaji lst = match lst with
    [] -> ""
  | {kanji = k; romaji = r} :: rest ->
    if romaji = r then k
    else romaji_to_kanji romaji rest

(* テスト *)
let test1 = romaji_to_kanji "myogadani" global_ekimei_list = "茗荷谷"
let test2 = romaji_to_kanji "akihabara" global_ekimei_list = "秋葉原"
let test3 = romaji_to_kanji "akibahara" global_ekimei_list = ""
