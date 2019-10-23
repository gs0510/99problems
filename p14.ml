let rec duplicate l =
	match l with 
		[] -> [] |
		h::t -> h::h::duplicate t
;;

assert (duplicate ["a";"b";"c";"c";"d"] = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]);;
