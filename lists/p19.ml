let rec take l n =
	match l with
	[] -> [] |
	h::t -> if n = 0 then [] else h :: take t (n-1)
;;

let rec drop l n =
	match l with
	[] -> [] |
	h::t -> if n = 0 then l else drop t (n-1)
;;


let rotate l n =
	if n >= 0 then
		(drop l n)@(take l n)
	else 
		(drop l ((List.length l)+n))@(take l ((List.length l)+n))
;;

assert (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3 = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]);;

assert (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2) = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]);;
	
