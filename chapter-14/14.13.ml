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

(* 目的：未確定の駅のリスト v を必要に応じて更新したリストを返す *)
(* koushin : eki_t -> eki_t list -> eki_t list *)
let koushin p v =
  List.map (fun q -> match (p, q) with
        ({namae = pn; saitan_kyori = ps; temae_list = pt},
         {namae = qn; saitan_kyori = qs; temae_list = qt}) ->
        let kyori = get_ekikan_kyori pn qn global_ekikan_list in
        if kyori = infinity
        then q
        else if ps +. kyori < qs
        then {namae = qn; saitan_kyori = ps +. kyori;
              temae_list = qn :: pt}
        else q)
    v

(* または *)

(* 目的：未確定の駅のリスト v を必要に応じて更新したリストを返す *)
(* koushin : eki_t -> eki_t list -> eki_t list *)
let koushin p v = match p with
    {namae = pn; saitan_kyori = ps; temae_list = pt} ->
    List.map (fun q -> match q with
          {namae = qn; saitan_kyori = qs; temae_list = qt} ->
          let kyori = get_ekikan_kyori pn qn global_ekikan_list in
          if kyori = infinity
          then q
          else if ps +. kyori < qs
          then {namae = qn; saitan_kyori = ps +. kyori;
                temae_list = qn :: pt}
          else q)
      v

(* 駅の例 *)
let eki1 = {namae="池袋"; saitan_kyori = infinity; temae_list = []}
let eki2 = {namae="新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"]}
let eki3 = {namae="茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]}
let eki4 = {namae="後楽園"; saitan_kyori = infinity; temae_list = []}

(* 駅リストの例 *)
let lst = [eki1; eki2; eki3; eki4]

(* テスト *)
let test1 = koushin eki2 [] = []
let test2 = koushin eki2 lst =
            [{namae="池袋"; saitan_kyori = 3.0; temae_list = ["池袋"; "新大塚"; "茗荷谷"]};
             eki2; eki3; eki4]

