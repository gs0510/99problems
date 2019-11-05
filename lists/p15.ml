let replicate l n =
	let rec repeat x count acc = if count = 0 then acc else repeat x (count-1) (x::acc) in 
	let rec aux res l =
		match l with
		[] -> res |
		x::t -> (repeat x n res)
	in aux [] (List.rev l);;

assert ((replicate ["a";"b";"c"] 3) = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]);;
