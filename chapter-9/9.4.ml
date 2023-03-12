(* 目的: 受け取ったlstの長さを求める *)
(* length : ’a list -> int *)
let rec length lst = match lst with
    [] -> 0
  | first :: rest -> 1 + length rest

(* テスト *)
let test1 = length [] = 0
let test2 = length [2; 1; 3; 4; 7] = 5
let test3 = length [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] = 10
