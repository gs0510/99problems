let rec at n l =
     match l with
     [] -> None |
     h::t -> if n = 1  then Some h else at (n-1) t
;;

assert (at 3 [ "a" ; "b"; "c"; "d"; "e" ] = Some "c");;
assert (at 3 [ "a" ] = None);;
