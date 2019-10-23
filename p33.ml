let rec gcd c d =
	if d = 0 then c else gcd d (c mod d);;

let coprime a b =
	if gcd a b = 1 then true else false;;

assert (coprime 13 27);;
assert (not (coprime 20536 7826));;
