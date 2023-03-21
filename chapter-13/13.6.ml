(* global_ekikan_listを使う *)
# use "../chapter-9/9.10.ml"

(* グラフの中の節（駅）を表す型 *)
type eki_t = {
  namae        : string;       (* 駅名（漢字） *)
  saitan_kyori : float;        (* 最短距離 *)
  temae_list   : string list;  (* 手前の駅名（漢字）のリスト *)
}

(* 目的: 漢字の駅名を二つと駅間リストを受け取ったら2駅間の距離を返す *)
(* get_ekikan_kyori : string -> string -> ekikan_t list -> float *)
let rec get_ekikan_kyori ekimei1 ekimei2 lst = match lst with
    [] -> infinity
  | {kiten = k; shuten = s; kyori = kyori} :: rest ->
    if (ekimei1 = k && ekimei2 = s) || (ekimei1 = s && ekimei2 = k) then kyori
    else get_ekikan_kyori ekimei1 ekimei2 rest

(* 駅の例 *)
let eki1 = {namae="池袋"; saitan_kyori = infinity; temae_list = []}
let eki2 = {namae="新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"]}
let eki3 = {namae="茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]}
let eki4 = {namae="後楽園"; saitan_kyori = infinity; temae_list = []}

(* 目的: 未確定の駅 q を必要に応じて更新した駅を返す *)
(* koushin1 : eki_t -> eki_t -> eki_t *)
(* let koushin1 p q =
   match p with {namae = namae_p; saitan_kyori = saitan_kyori_p; temae_list = temae_list_p} as eki_p ->
   match q with {namae = namae_q; saitan_kyori = saitan_kyori_q; temae_list = temae_list_q} as eki_q ->
    let kyori = get_ekikan_kyori namae_p namae_q global_ekikan_list in
    if kyori = infinity then eki_q
    else if saitan_kyori_p +. kyori < saitan_kyori_q then {namae = namae_q; saitan_kyori = saitan_kyori_p +. kyori; temae_list = namae_q :: temae_list_p}
    else eki_q *)
let koushin1 p q = match (p, q) with
    ({namae = pn; saitan_kyori = ps; temae_list = pt},
     {namae = qn; saitan_kyori = qs; temae_list = qt}) ->
    let kyori = get_ekikan_kyori pn qn global_ekikan_list in
    if kyori = infinity
    then q
    else if ps +. kyori < qs
    then {namae = qn; saitan_kyori = ps +. kyori; temae_list = qn :: pt}
    else q


(* テスト *)
let test1 = koushin1 eki3 eki1 = eki1
let test2 = koushin1 eki3 eki2 = eki2
let test3 = koushin1 eki3 eki3 = eki3
let test4 = koushin1 eki3 eki4 =
            {namae="後楽園"; saitan_kyori = 1.8; temae_list = ["後楽園"; "茗荷谷"]}
let test5 = koushin1 eki2 eki1 =
            {namae="池袋"; saitan_kyori = 3.0; temae_list = ["池袋"; "新大塚"; "茗荷谷"]}
let test6 = koushin1 eki2 eki2 = eki2
let test7 = koushin1 eki2 eki3 = eki3
let test8 = koushin1 eki2 eki4 = eki4

(* 13.7 *)
(* 目的：未確定の駅のリスト v を必要に応じて更新したリストを返す *)
(* koushin : eki_t -> eki_t list -> eki_t list *)
let koushin p v =
  let f q = koushin1 p q in
  List.map f v

(* 駅リストの例 *)
let lst = [eki1; eki2; eki3; eki4]

(* テスト *)
let test9 = koushin eki2 [] = []
let test10 = koushin eki2 lst =
             [{namae="池袋"; saitan_kyori = 3.0; temae_list = ["池袋"; "新大塚"; "茗荷谷"]};
              eki2; eki3; eki4]
