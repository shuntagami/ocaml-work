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
let lst1 = [person1; person2; person3]

(* 目的：person_t 型のデータから名前を返す *)
(* take_name : person_t -> string *)
let take_name person = match person with
    {name = n; shincho = s; taiju = t; tsuki = ts; hi = h; ketsueki = k} -> n

(* テスト *)
let test1 = take_name person1 = "浅井"
let test2 = take_name person2 = "宮原"
let test3 = take_name person3 = "中村"

(* 目的: person_t型のリストを受け取ったらその中に出てくる人の名前のリストを返す *)
(* person_namae : person_t list -> string list *)
let person_namae lst = List.map take_name lst

(* テスト *)
let test1 = person_namae lst1 = ["浅井"; "宮原"; "中村"]
