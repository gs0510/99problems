let rec length_inner l n =
        match l with 
        [] -> n |
        _::t -> length_inner t (n+1)
;;

let length l =
        length_inner l 0
;;

assert (length [ "a" ; "b" ; "c"] = 3);;
assert (length [] = 0);;
