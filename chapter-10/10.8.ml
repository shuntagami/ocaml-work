(* 人に関する情報を格納するレコード *)
type person_t = {
  name : string; (* 名前 *)
  shincho : float; (* 身長 *)
  taiju : float; (* 体重 *)
  tsuki : int; (* 誕生月 *)
  hi : int; (* 誕生日 *)
  ketsueki : string; (* 血液型 *)
}

let person1 =
  {name = "浅井";
   shincho = 1.72;
   taiju = 58.5;
   tsuki = 9;
   hi = 17;
   ketsueki = "A"
  }

let person2 = {
  name = "宮原";
  shincho = 1.63;
  taiju = 55.0;
  tsuki = 6;
  hi = 30;
  ketsueki = "B"
}

let person3 = {
  name = "中村";
  shincho = 1.68;
  taiju = 63.0;
  tsuki = 6;
  hi = 6;
  ketsueki = "O"
}

(* person_t 型のリストの例 *)
let lst1 = [person1]
let lst2 = [person2]
let lst3 = [person1; person2; person3]
let lst4 = [person2; person1; person1]

(* 目的: person_t型のlstのうち各血液型の人が何人いるかを集計する *)
(* ketsueki_shukei : person_t list -> int * int * int * int *)
let rec ketsueki_shukei lst = match lst with
    [] -> (0, 0, 0, 0)
  | {name = n; shincho = s; taiju = t; tsuki = ts; hi = h; ketsueki = k}  :: rest ->
    let (a, b, o, ab) = ketsueki_shukei rest in
    if k = "A" then (a + 1, b, o, ab)
    else if k = "B" then (a, b + 1, o, ab)
    else if k = "O" then (a, b, o + 1, ab)
    else (a, b, o, ab + 1)

(* 目的: person_t型のlstのうち最も人数の多かった血液型を返す *)
(* saita_ketsueki : person_t list -> string *)
let saita_ketsueki lst =
  let (a, b, o, ab) = ketsueki_shukei lst in
  let saidai = max (max a b) (max o ab) in
  if saidai = a then "A"
  else if saidai = b then "B"
  else if saidai = o then "O"
  else "AB"

(* テスト *)
let test1 = saita_ketsueki lst1 = "A"
let test2 = saita_ketsueki lst2 = "B"
let test3 = saita_ketsueki lst3 = "A"
(* 同点のときは A, O, B, AB の順に見つかったものが返されてしまう *)
let test4 = saita_ketsueki lst4 = "A"
