let rec rev l =
        match l with 
        [] -> [] |
        h::t -> rev t @ [h];;

assert (rev ["a" ; "b" ; "c"] = ["c"; "b"; "a"]);;
