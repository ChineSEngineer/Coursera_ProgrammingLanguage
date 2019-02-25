datatype 'a mylist = Cons of 'a * ('a mylist) | Empty

fun map f xs =
    case xs of
	Empty => Empty
      | Cons(x, xs) => Cons (f x, map f xs)

fun filter f xs =
    case xs of
	Empty => Empty
      | Cons(x, xs) => Cons(f x, map f xs)

fun length xs =
    case xs of
	Empty => 0
      | Cons(_, xs) => 1 + Length xs

val doubleAll = map (fn x => 2 * x)

fun countNs (xs, n : int) = length (filter (fn x => x = n) xs)
				 
