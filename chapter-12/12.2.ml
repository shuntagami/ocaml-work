(* 駅名の情報を格納するレコード型 *)
type ekimei_t = {
  kanji   : string; (* 漢字の駅名 *)
  kana    : string; (* 読み *)
  romaji  : string; (* ローマ字 *)
  shozoku : string; (* 所属路線名 *)
}

(* グラフの中の節（駅）を表す型 *)
type eki_t = {
  namae        : string;       (* 駅名（漢字） *)
  saitan_kyori : float;        (* 最短距離 *)
  temae_list   : string list;  (* 手前の駅名（漢字）のリスト *)
}

(* 駅名リストの例 *)
let ekimei_list = [
  {kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="丸ノ内線"};
  {kanji="新大塚"; kana="しんおおつか"; romaji="shinotsuka"; shozoku="丸ノ内線"};
  {kanji="茗荷谷"; kana="みょうがだに"; romaji="myogadani"; shozoku="丸ノ内線"};
  {kanji="後楽園"; kana="こうらくえん"; romaji="korakuen"; shozoku="丸ノ内線"};
  {kanji="本郷三丁目"; kana="ほんごうさんちょうめ"; romaji="hongosanchome"; shozoku="丸ノ内線"};
  {kanji="御茶ノ水"; kana="おちゃのみず"; romaji="ochanomizu"; shozoku="丸ノ内線"}
]

(* 目的: ekimei_t 型のリストを受け取ったらその駅名を使ってeki_t 型のリストを作る *)
(* make_eki_list : ekimei_t list -> eki_t list *)
let rec make_eki_list lst = match lst with
    [] -> []
  | {kanji = kanji; kana = kana; romaji = romaji; shozoku = shozoku} :: rest ->
    {namae = kanji; saitan_kyori = infinity; temae_list = []} :: make_eki_list rest

(* テスト *)
let test1 = make_eki_list [] = []
let test2 = make_eki_list ekimei_list = [
    {namae="池袋"; saitan_kyori = infinity; temae_list = []};
    {namae="新大塚"; saitan_kyori = infinity; temae_list = []};
    {namae="茗荷谷"; saitan_kyori = infinity; temae_list = []};
    {namae="後楽園"; saitan_kyori = infinity; temae_list = []};
    {namae="本郷三丁目"; saitan_kyori = infinity; temae_list = []};
    {namae="御茶ノ水"; saitan_kyori = infinity; temae_list = []}
  ]
