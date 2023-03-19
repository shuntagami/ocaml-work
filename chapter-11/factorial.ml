(* 目的：自然数n の階乗を求める*)
(* factorial : int -> int *)
let rec factorial n =
  if n = 0 then 1
  else n * factorial (n - 1)

(* テスト*)
let test1 = factorial 0 = 1
let test2 = factorial 1 = 1
let test3 = factorial 2 = 2
let test4 = factorial 3 = 6
let test5 = factorial 4 = 24
let test6 = factorial 10 = 3628800
