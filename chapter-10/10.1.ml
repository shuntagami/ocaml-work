(* 目的: 昇給に並んでいる整数のlstと整数nを受け取りnを挿入したリストを返す *)
(* insert : int list -> int -> int list *)
let rec insert lst n = match lst with
    [] -> [n]
  | first :: rest -> if n <= first then n :: lst
    else first :: insert rest n

(* テスト *)
let test1 = insert [] 3 = [3]
let test2 = insert [1] 3 = [1; 3]
let test3 = insert [3] 1 = [1; 3]
let test4 = insert [1; 3; 4; 7; 8] 5 = [1; 3; 4; 5; 7; 8]

(* 目的: 受け取ったlstを昇順に整列して返却する、挿入法 *)
(* ins_sort : int list -> int list *)
let rec ins_sort lst = match lst with
    [] -> []
  | first :: rest -> insert (ins_sort rest) first

(* テスト *)
let test5 = ins_sort [] = []
let test6 = ins_sort [1] = [1]
let test7 = ins_sort [5; 3; 9; 0; 4] = [0; 3; 4; 5; 9]
let test8 = ins_sort [-5; -10; 10; 0; 1] = [-10; -5; 0; 1; 10]
