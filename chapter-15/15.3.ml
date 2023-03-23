(* 目的: 2以上n以下の自然数のリストを受け取ったら2以上n以下の素数のリストを返す *)
(* sieve : int list -> int list *)
let rec sieve lst = match lst with
    [] -> []
  | first :: rest ->
    first :: sieve (List.filter (fun n -> n mod first != 0) rest)

(* テスト *)
let test1 = sieve [2] = [2]
let test2 = sieve [2; 3] = [2; 3]
let test3 = sieve [2; 3; 4; 5; 6; 7; 8; 9; 10] = [2; 3; 5; 7]

(* 目的：2からnまでのリストを作る*)
(* enumerate : int -> int list *)
let rec two_to_n n =
  if n <= 2 then [2] else two_to_n (n - 1) @ [n]

(* 目的: 自然数を受け取ったらそれ以下の素数のリストを返す *)
(* prime : int -> int list *)
let prime n = sieve (two_to_n n)

(* テスト *)
let test4 = prime 2 = [2]
let test5 = prime 3 = [2; 3]
let test6 = prime 10 = [2; 3; 5; 7]
