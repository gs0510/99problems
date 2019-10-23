let rec remove_at n l =
	match l with 
	[] -> []
	| h::t -> if n = 0 then t else h :: remove_at (n-1) t
;;

assert (remove_at 1 ["a";"b";"c";"d"] = ["a"; "c"; "d"]);;
