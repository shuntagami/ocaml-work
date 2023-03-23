(* グラフの中の節（駅）を表す型 *)
type eki_t = {
  namae        : string;       (* 駅名（漢字） *)
  saitan_kyori : float;        (* 最短距離 *)
  temae_list   : string list;  (* 手前の駅名（漢字）のリスト *)
}

(* 自分の回答 *)
(* 目的: eki_t型のリストを受け取り「最短距離最小の駅」を返す *)
(* saitan : eki_t list -> eki_t *)
(* let rec saitan_no_eki lst = match lst with
    [] -> { namae=""; saitan_kyori=infinity; temae_list = [] }
  | { namae = n; saitan_kyori = s; temae_list = t } as eki_first :: rest ->
    let min_rest = saitan_no_eki rest in
    if s <= min_rest.saitan_kyori then eki_first
    else min_rest *)

(* let saitan_wo_bunri lst = match lst with
    [] -> ({ namae =""; saitan_kyori = infinity; temae_list = []}, [])
  | first :: rest ->
    let tmp_saitan = saitan_no_eki lst in
    (tmp_saitan, List.filter (fun eki -> eki != tmp_saitan) lst) *)

(* 模範回答 *)
(* 目的：受け取った駅のリストを、最短距離最小の駅とそれ以外に分離する *)
(* saitan_wo_bunri : eki_t list -> eki_t * eki_t list *)
(* let rec saitan_wo_bunri eki_list = match eki_list with
    [] -> ({namae = ""; saitan_kyori = infinity; temae_list = []}, [])
  | first :: rest ->
      let (p, v) = saitan_wo_bunri rest in
      match (first, p) with
	({namae = fn; saitan_kyori = fs; temae_list = ft},
	 {namae = pn; saitan_kyori = ps; temae_list = pt}) ->
	  if fs < ps then (first, rest)
	  else (p, first :: v) *)

(* 15.5: saitan_wo_bunriをfold_rightを使って書き直す *)
let saitan_wo_bunri eki_list = match eki_list with
    [] -> ({namae = ""; saitan_kyori = infinity; temae_list = []}, [])
  | first :: rest ->
      List.fold_right (fun first (p, v) ->
			 match (first, p) with
			   ({namae = fn; saitan_kyori = fs; temae_list = ft},
			    {namae = sn; saitan_kyori = ss; temae_list = st}) ->
			     if fs < ss then (first, p :: v)
			     else (p, first :: v))
		      rest
		      (first, [])

(* 駅の例 *)
let eki1 = {namae="池袋"; saitan_kyori = infinity; temae_list = []}
let eki2 = {namae="新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"]}
let eki3 = {namae="茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]}
let eki4 = {namae="後楽園"; saitan_kyori = infinity; temae_list = []}

(* 駅リストの例 *)
let lst = [eki1; eki2; eki3; eki4]

(* テスト *)
let test = saitan_wo_bunri lst = (eki3, [eki2; eki1; eki4])
