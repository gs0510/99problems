type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let encode l =
	let compact count x = if count = 1 then One x else Many (count+1, x) in
	let rec aux res count l =
		match l with
		[] -> [] |
		[x] -> compact count x :: res |
		a::(b::_ as t) -> if a = b then aux res (count+1) t 
						  else aux (compact count a :: res) 0 t 
		in List.rev (aux [] 0 l);;

assert (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")]);; 
		 
