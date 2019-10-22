let rec aux n res l=
	match l with
	[] -> [] |
	[x] -> (n+1, x)::res |
	a::(b::_ as t) -> if a = b then aux (n+1) res t
		else aux 0 ( (n+1, a)::res) t
;; 


let encode l =
     List.rev (aux 0 [] l)
;;


assert (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")])
;;
