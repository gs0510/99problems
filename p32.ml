let rec gcd c d =
		if d = 0 then c else gcd d (c mod d)
;;

assert (gcd 13 27 = 1);;
assert (gcd 20536 7826 = 2);;
