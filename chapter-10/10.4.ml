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
let lst1 = []
let lst2 = [person1]
let lst3 = [person1; person2]
let lst4 = [person2; person1]
let lst5 = [person3; person1; person2]

(* 目的：昇順に並んでいる lst の正しい位置に person を挿入する *)
(* person_insert : person_t list -> person_t -> person_t list *)
let rec person_insert lst person0 = match lst with
    [] -> [person0]
  | ({name = n; shincho = s; taiju = t; tsuki = ts; hi = h; ketsueki = k}
     as person) :: rest ->
      match person0 with
        {name = n0; shincho = s0; taiju = t0;
	 tsuki = ts0; hi = h0; ketsueki = k0} ->
	  if n < n0 then person :: person_insert rest person0
	  else person0 :: lst

(* テスト *)
let test1 = person_insert [] person1 = [person1]
let test2 = person_insert [person2] person1 = [person2; person1]
let test3 = person_insert [person1] person2 = [person2; person1]
let test4 = person_insert [person1; person2] person3
	    = [person1; person2; person3]

(* 目的：受け取った人のリスト lst を名前の順に並べる *)
(* person_ins_sort : person_t list -> person_t list *)
let rec person_ins_sort lst = match lst with
    [] -> []
  | first :: rest -> person_insert (person_ins_sort rest) first

(* テスト *)
let test5 = person_ins_sort lst1 = []
let test6 = person_ins_sort lst2 = [person1]
let test7 = person_ins_sort lst3 = [person2; person1]
let test8 = person_ins_sort lst4 = [person2; person1]
let test9 = person_ins_sort lst5 = [person2; person1; person3]
