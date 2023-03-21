(* 目的：リストlst の中から条件p を満たす要素のみを取り出す*)
(* filter : (’a -> bool) -> ’a list -> ’a list *)
let rec filter p lst = match lst with
    [] -> []
  | first :: rest -> if p first then first :: filter p rest
    else filter p rest

(* 目的: リストlstから偶数の要素のみを取り出す *)
(* even : int list -> int list *)
let even lst =
  let is_even n = n mod 2 = 0 in
  filter is_even lst

(* テスト *)
let test1 = even [] = []
let test2 = even [1; 3; 5] = []
let test2 = even [1; 2; 3; 4; 5] = [2; 4]
