# use "./10.10.ml"
      # use "./10.11.ml"

(* 目的: ローマ字の駅名を二つ受け取りメッセージを返す *)
(* kyori_wo_hyoji : string -> string -> string *)
let kyori_wo_hyoji ekimei1 ekimei2 =
  let ekimei1_to_kanji = romaji_to_kanji ekimei1 global_ekimei_list in
  if ekimei1_to_kanji = "" then ekimei1 ^ "という駅は存在しません"
  else let ekimei2_to_kanji = romaji_to_kanji ekimei2 global_ekimei_list in
    if ekimei2_to_kanji = "" then ekimei2 ^ "という駅は存在しません"
    else let kyori = get_ekikan_kyori ekimei1_to_kanji ekimei2_to_kanji global_ekikan_list in
      if kyori = infinity
      then ekimei1_to_kanji ^ "駅と" ^ ekimei2_to_kanji ^ "駅はつながっていません"
      else ekimei1_to_kanji ^ "駅から" ^ ekimei2_to_kanji ^ "駅までは" ^ string_of_float kyori ^ "kmです"

(* テスト *)
let test1 = kyori_wo_hyoji "kodemmacho" "akihabara" = "小伝馬町駅から秋葉原駅までは0.9kmです"
let test2 = kyori_wo_hyoji "akihabara" "kodemmacho"  = "秋葉原駅から小伝馬町駅までは0.9kmです"
let test3 = kyori_wo_hyoji "akihabara" "nakaokachimachi"  = "秋葉原駅から仲御徒町駅までは1.kmです"
let test4 = kyori_wo_hyoji "akihabara" "kanda"  = "秋葉原駅と神田駅はつながっていません"
let test5 = kyori_wo_hyoji "akihabara" "sendai" = "sendaiという駅は存在しません"
