(* 目的: (x, y)の組で表された平面座標を受け取り、x軸について対称な点の座標を返す *)
(* taisho : float * float -> float * float *)
let taisho_x pair = match pair with
    (x, y) -> (x, -. y)

(* テスト *)
let test1 = taisho_x (0.0, 0.0) = (0.0, 0.0)
let test2 = taisho_x (2.3, 5.1) = (2.3, -5.1)
let test3 = taisho_x (-3.8, -2.4) = (-3.8, 2.4)
