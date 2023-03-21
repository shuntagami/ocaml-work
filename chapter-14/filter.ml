(* 目的：受け取ったリストlst から正の要素のみを取り出す*)
(* filter_positive : int list -> int list *)
let rec filter_positive lst = match lst with
    [] -> []
  | first :: rest ->
    if first > 0 then first :: filter_positive rest
    else filter_positive rest

(* 目的：リストlst から3 で割ると1 余る要素のみを取り出す*)
(* filter_mod3_1 : int list -> int list *)
let rec filter_mod3_1 lst = match lst with
    [] -> []
  | first :: rest ->
    let is_mod3_1 n = n mod 3 = 1 in
    if is_mod3_1 first then first :: filter_mod3_1 rest
    else filter_mod3_1 rest

(* 目的：リストlst の中から条件p を満たす要素のみを取り出す*)
(* filter : (’a -> bool) -> ’a list -> ’a list *)
let rec filter p lst = match lst with
    [] -> []
  | first :: rest -> if p first then first :: filter p rest
    else filter p rest

(* 目的：リストlst から3 で割ると1 余る要素のみを取り出す*)
(* filter_mod3_1 : int list -> int list *)
let filter_mod3_1 lst =
  let is_mod_3_1 n =
    n mod 3 = 1 in
  filter is_mod_3_1 lst

(* 目的：受け取ったリストlst から正の要素のみを取り出す*)
(* filter_positive : int list -> int list *)
let filter_positive lst =
  let is_positive n =
    n > 0 in
  filter is_positive lst

