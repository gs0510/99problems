let rec drop l n =
	match l with
	[] -> [] |
	h::t -> if n = 0 then l
			else drop t (n-1)
;;

let rec take l n =
	match l with 
	[] -> [] |
	h::t -> if n = 0 then []
			else h :: take t (n-1)
;;
	
let slice l startl endl =
	take (drop l startl)  (endl - startl +1)
;;

assert (slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6 = ["c"; "d"; "e"; "f"; "g"]);;
	
