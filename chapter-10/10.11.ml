# use "../chapter-9/9.10.ml"

(* 目的: 漢字の駅名を二つと駅間リストを受け取ったら2駅間の距離を返す *)
(* get_ekikan_kyori : string -> string -> ekikan_t list -> float *)
let rec get_ekikan_kyori ekimei1 ekimei2 lst = match lst with
    [] -> infinity
  | {kiten = k; shuten = s; kyori = kyori} :: rest ->
    if (ekimei1 = k && ekimei2 = s) || (ekimei1 = s && ekimei2 = k) then kyori
    else get_ekikan_kyori ekimei1 ekimei2 rest

(* テスト *)
let test1 = get_ekikan_kyori "小伝馬町" "秋葉原" global_ekikan_list = 0.9
let test2 = get_ekikan_kyori "秋葉原" "小伝馬町" global_ekikan_list = 0.9
let test3 = get_ekikan_kyori "秋葉原" "仲御徒町" global_ekikan_list = 1.
let test3 = get_ekikan_kyori "秋葉原" "池袋" global_ekikan_list = infinity
