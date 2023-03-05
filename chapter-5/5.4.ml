(* 目的: 2 次方程式ax2 + bx + c = 0 の係数a, b, c（いずれも実数とする）を与えられたら、判別式の値を返す *)
(* hanbetsushiki a -> b -> c -> float : *)
let hanbetsushiki a b c = b ** 2.0 -. 4.0 *. a *. c

(* テスト *)
let test1 = hanbetsushiki 1.0 5.0 4.0 = 9.0
let test2 = hanbetsushiki 2.0 (-4.0) 2.0 = 0.0
let test3 = hanbetsushiki 1.0 2.0 4.0 = -12.0

