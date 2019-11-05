let rec aux res l =
        match l with 
        [] -> res |
        h::t -> if h <> List.nth res 0 then h:: (aux res t) else (aux res t) 
;;

let compress l =
	List.rev (aux [] l)
;;
	

assert (compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = ["a"; "b"; "c"; "a"; "d"; "e"]);;
