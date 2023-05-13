(* 距離と距離の合計を持っている型*)
type distance_t = {
  kyori : float; (* 距離*)
  total : float; (* 距離の合計*)
}

(* 目的：先頭からリスト中の各点までの距離の合計を計算する*)
(* ここでtotal0 はこれまでの距離の合計*)
(* hojo : distance_t list -> float -> distance_t list *)
(* let rec hojo lst total0 = match lst with
    [] -> []
   | {kyori = k; total = t} :: rest ->
    {kyori = k; total = total0 +. k}
    :: hojo rest (total0 +. k) *)

(* 目的：先頭からリスト中の各点までの距離の合計を計算する*)
(* total_distance : distance_t list -> distance_t list *)
(* let total_distance lst = hojo lst 0.0 *)

(* hojoを局所関数にしてtotal_distanceを書き直し *)
(* 目的：先頭からリスト中の各点までの距離の合計を計算する*)
(* total_distance : distance_t list -> distance_t list *)
let total_distance lst =
  (* 目的：先頭からリスト中の各点までの距離の合計を計算する*)
  (* ここでtotal0 はこれまでの距離の合計（アキュムレータ）*)
  (* hojo : distance_t list -> float -> distance_t list *)
  let rec hojo lst total0 = match lst with
      [] -> []
    | {kyori = k; total = t} :: rest ->
      {kyori = k; total = total0 +. k}
      :: hojo rest (total0 +. k)
  in hojo lst 0.0

let lst = [
  {kyori = 0.3; total = 0.};
  {kyori = 0.9; total = 0.};
  {kyori = 1.4; total = 0.};
  {kyori = 0.8; total = 0.};
]

let test = total_distance lst = [
  {kyori = 0.3; total = 0.3};
  {kyori = 0.9; total = 1.2};
  {kyori = 1.4; total = 2.5999999999999996};
  {kyori = 0.8; total = 3.3999999999999995};
]
