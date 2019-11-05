let rec last l =
        match l with 
        [] -> None
        | [x] -> Some x 
        | h::t -> last t
;;

assert (last [`a; `b; `c; `d] = Some `d);;
assert (last [] = None);;
