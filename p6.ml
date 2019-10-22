let rec rev l = 
        match l with
        [] -> [] |
        h::t -> rev t @ [h];;


let is_palindrome l = 
        if rev l = l then true
        else false;;

assert (is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ] = true);;
assert (is_palindrome [ "a" ; "b" ] = false);;
