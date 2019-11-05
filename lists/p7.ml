type 'a node =
   | One of 'a 
   | Many of 'a node list;;

let rec func result l=
    match l with
    [] -> result |
    One x::t -> func (x :: result) t |
    Many y::t -> func ( func result y) t
;;

let flatten l = 
    func [] l
;;

assert (flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ] = ["a"; "b"; "c"; "d"; "e"]);;


