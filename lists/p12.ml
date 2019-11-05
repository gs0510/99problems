type 'a rle =
    | One of 'a
    | Many of int * 'a
;;

let decode l =
	let rec many res n temp =
		if n = 0 then res else many (temp::res) (n-1) temp in
	let rec aux l res = 
		match l with 
		[] -> res |
		One x::t -> aux t (x::res) |
		Many (n, x)::t -> aux t (many res n x)
	in aux (List.rev l) []
;;

assert (decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")] = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]);;
