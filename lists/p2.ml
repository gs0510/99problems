let rec last_two l =
        match l with 
        [] -> None |
        [x] -> None |
        [x; y] -> Some (x, y)|
        h::t -> last_two t
;;

assert( last_two ["a"; "b"; "c"; "d"] = Some("c", "d"));;
assert( last_two [`a] = None);;
