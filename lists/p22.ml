let range startl endl =
	let rec aux a b =
		if a > b then []
		else a :: aux (a+1) b
	in if startl > endl then List.rev (aux endl startl) 
	   else aux startl endl
;;

assert (range 4 9 = [4; 5; 6; 7; 8; 9]);;
assert (range 9 4 = [9; 8; 7; 6; 5; 4]);;
