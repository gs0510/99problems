let split l n=
	let rec aux l i res =
		match l with 
		[] -> List.rev res, []
		| h::t as l -> if i = 0 then List.rev res, l
					   else aux t (i-1) (h::res) 
		in aux l n []
;;

assert (split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]));;

assert (split ["a";"b";"c";"d"] 5 = (["a"; "b"; "c"; "d"], []));;
