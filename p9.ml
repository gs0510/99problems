let rec aux res curr l = 
	match l with
	[] -> [] |
	[x] -> (x :: curr) :: res |
	a::(b::_ as t) -> if a = b then aux res (a::curr) t
		else (aux ( (a::curr) :: res) [] t) 
;;


let pack l =
	List.rev (aux [] [] l)
;;

assert (pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"] = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]]);;
