(* 目的：与えられたリストを逆順にして返す*)
(* reverse : ’a list -> ’a list *)
let reverse lst =
  (* 目的：(lst の逆順のリスト) @ result を返す*)
  (* ここでresult はこれまでの要素を逆順にしたリストを示す*)
  (* ex: [1; 2; 3; 4; 5]のリストを見る。今[1; 2; 3]まで見て、これから[4; 5]を処理するところ *)
  (* すると、この時点ではresultは[3; 2; 1]でlstが[4; 5]になる *)
  let rec rev lst result = match lst with
      [] -> result
    | first :: rest -> rev rest (first :: result)
  in rev lst []

let test1 = reverse [] = []
let test2 = reverse [1; 2; 3] = [3; 2; 1]
let test3 = reverse [1; 1; 1; 1] = [1; 1; 1; 1]
