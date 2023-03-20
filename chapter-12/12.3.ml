(* # use "./12.2.ml" *)

(* グラフの中の節（駅）を表す型 *)
type eki_t = {
  namae        : string;       (* 駅名（漢字） *)
  saitan_kyori : float;        (* 最短距離 *)
  temae_list   : string list;  (* 手前の駅名（漢字）のリスト *)
}

(* 駅リストの例 *)
let eki_list = [
  {namae="池袋"; saitan_kyori = infinity; temae_list = []};
  {namae="新大塚"; saitan_kyori = infinity; temae_list = []};
  {namae="茗荷谷"; saitan_kyori = infinity; temae_list = []};
  {namae="後楽園"; saitan_kyori = infinity; temae_list = []};
  {namae="本郷三丁目"; saitan_kyori = infinity; temae_list = []};
  {namae="御茶ノ水"; saitan_kyori = infinity; temae_list = []}
]

(* 目的: eki_t型のリストと起点を受け取ったら新しいeki_t型のリストを返す *)
(* shokika : eki_t list -> string -> eki_t list *)
let rec shokika lst kiten = match lst with
    [] -> []
  | {namae = n; saitan_kyori = s; temae_list = t} as first :: rest ->
    if n = kiten then {namae = n; saitan_kyori = 0.0; temae_list = [n]} :: shokika rest kiten
    else first :: shokika rest kiten

(* テスト *)
let test1 = shokika [] "茗荷谷" = []
let test2 = shokika eki_list "茗荷谷" = [
    {namae="池袋"; saitan_kyori = infinity; temae_list = []};
    {namae="新大塚"; saitan_kyori = infinity; temae_list = []};
    {namae="茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]};
    {namae="後楽園"; saitan_kyori = infinity; temae_list = []};
    {namae="本郷三丁目"; saitan_kyori = infinity; temae_list = []};
    {namae="御茶ノ水"; saitan_kyori = infinity; temae_list = []}
  ]
