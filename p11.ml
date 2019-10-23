type 'a rle =
    | One of 'a
    | Many of int * 'a;;


let rec aux num result l =
	let create_tuple count elem =
		if count = 1 then One elem
		else Many (count, elem) in
	match l with
	[] -> [] |
	[x] -> (create_tuple (num+1) x) :: result |
	a::(b::_ as t) -> 
		if a = b then aux (num+1) result t
		else aux 0 ((create_tuple (num+1) a) :: result) t 
;;

let encode l =
    List.rev (aux 0 [] l);;


assert (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")]);;
