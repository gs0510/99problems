let drop l n =
	let rec aux l i =
		match l with
		[] -> [] |
		x::t -> if n = i then aux t 1 else x::(aux t (i+1))
	in aux l 1
;;

assert (drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]);;
