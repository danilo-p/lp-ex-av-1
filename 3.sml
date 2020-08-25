datatype expr =
    IConst of int
    | Div of expr * expr
    | Mult of expr * expr
    | Plus of expr * expr
    | Minus of expr * expr;

fun eval (IConst i) = i
  | eval (Div(e1, e2)) =
    let val res = (eval e2) in
        if res = 0 then
            0
        else
            (eval e1) div res
    end
  | eval (Mult(e1, e2)) = (eval e1) * (eval e2)
  | eval (Plus(e1, e2)) = (eval e1) + (eval e2)
  | eval (Minus(e1, e2)) = (eval e1) - (eval e2);
