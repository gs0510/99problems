type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let rec eval a val_a b val_b = function
  | Var x ->
      if x = a then val_a
      else if x = b then val_b
      else failwith "Invalid_argument"
  | Not e -> not (eval a val_a b val_b e)
  | And (e1, e2) -> eval a val_a b val_b e1 && eval a val_a b val_b e2
  | Or (e1, e2) -> eval a val_a b val_b e1 || eval a val_a b val_b e2

let table2 a b e =
  [
    (true, true, eval a true b true e);
    (true, false, eval a true b false e);
    (false, true, eval a false b true e);
    (false, false, eval a false b false e);
  ]

let _ =
  assert (
    table2 "a" "b" (And (Var "a", Or (Var "a", Var "b")))
    = [
        (true, true, true);
        (true, false, true);
        (false, true, false);
        (false, false, false);
      ] )
