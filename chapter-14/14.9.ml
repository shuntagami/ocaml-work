(* 人に関する情報を格納するレコード *)
type person_t = {
  name : string; (* 名前 *)
  shincho : float; (* 身長 *)
  taiju : float; (* 体重 *)
  tsuki : int; (* 誕生月 *)
  hi : int; (* 誕生日 *)
  ketsueki : string; (* 血液型 *)
}

(* person_t -> string *)
fun person -> match person with
  {name = n; shincho = s; taiju = t; tsuki = ts; hi = h; ketsueki = k} -> n
;;

(* または *)

(* person_t -> string *)
fun {name = n; shincho = s; taiju = t; tsuki = ts; hi = h; ketsueki = k} -> n
