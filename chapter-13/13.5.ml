(*
受け取った関数を４回、実行するような関数を返す関数。例えば、以下。

# let twice f =
    let g x = f (f x)
    in g ;;
val twice : ('a -> 'a) -> 'a -> 'a = <fun>
# let g = twice twice ;;
val g : ('_weak1 -> '_weak1) -> '_weak1 -> '_weak1 = <fun>
# val h : int -> int = <fun>
# h 3 ;;
- : int = 7
#

関数 h は (fun x -> x + 1) を４回、実行するような関数になっている。

関数 g の型は、

('a -> 'a) -> 'a -> 'a

という多相型になりそうなところだが、そうはならず

('_weak1 -> '_weak1) -> '_weak1 -> '_weak1

という特殊な型になる。この型は '_weak1 を一度だけ具体的な型にすることが
できるようなもので、実質的に単相な型である。実際、上のように一度、
int -> int 型の関数 (fun x -> x + 1) を g に渡すと、g の型は

# g ;;
- : (int -> int) -> int -> int = <fun>
#

に固定される。この後、g には int -> int 型以外の関数を渡すことはで
きない。このようになっているのは、多相性と後の章で説明する例外や参
照型が干渉するためである。g を多相関数として定義したい場合は、以下
のようにする。

# let g x = (twice twice) x ;;
val g : ('a -> 'a) -> 'a -> 'a = <fun>
#
*)

let twice f =
  let g x = f (f x)
  in g ;;

let g = twice twice
