(* 目的：init から始めて lst の要素を左から順に f を施し込む *)
(* fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)
(* fold_left f init [1; 2; 3; 4; 5] は
   f (f (f (f (f init 1) 2) 3) 4) 5
   の意味 *)
let rec fold_left f init lst = match lst with
    [] -> init
  | first :: rest -> fold_left f (f init first) rest

   (* テスト *)
let test1 = fold_left (-) 0 [] = 0
let test2 = fold_left (-) 10 [4; 1; 3] = 2
let test3 = fold_left (fun lst a -> a :: lst) [] [1; 2; 3; 4] = [4; 3; 2; 1]
