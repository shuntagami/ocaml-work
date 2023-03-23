(* 目的: m ≥ n ≥ 0 なる自然数mとnの最大公約数(Gratest Common Divisor, GCD)をユークリッドの互除法により求める *)
(* gcd : int -> int -> int *)
let rec gcd m n =
  if n = 0
  then m
  else gcd n (m mod n)

(* m を nで割った余りは必ずnより小さくなる、つまり必ずn = 0となり、停止する *)

(* テスト *)
let test1 = gcd 0 0 = 0
let test1 = gcd 1 0 = 1
let test1 = gcd 2 2 = 2
let test1 = gcd 4 2 = 2
let test1 = gcd 5 2 = 1
let test1 = gcd 10 4 = 2
