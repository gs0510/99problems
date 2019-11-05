let rec insert_at elem n l =
	match l with
	[] -> [] 
	| h::t -> if n = 0 then h:: elem :: t
			  else h :: insert_at elem (n-1) t
;;

assert (insert_at "alfa" 1 ["a";"b";"c";"d"] = ["a"; "alfa"; "b"; "c"; "d"]);;
assert (insert_at "alfa" 3 ["a";"b";"c";"d"] = ["a"; "b"; "c"; "alfa"; "d"]);;
assert (insert_at "alfa" 4 ["a";"b";"c";"d"] = ["a"; "b"; "c"; "d"; "alfa"]);;
