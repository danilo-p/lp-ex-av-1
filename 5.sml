datatype expr = 
    IConst of int
    | Div of expr * expr
    | Mult of expr * expr
    | Plus of expr * expr
    | Minus of expr * expr;

fun eval (IConst i) n = i mod n
  | eval (Div(e1, e2)) n =
    let val res = (eval e2 n) in
        if res = 0 then
            0
        else
            ((eval e1 n) div res) mod n
    end
  | eval (Mult(e1, e2)) n = ((eval e1 n) * (eval e2 n)) mod n
  | eval (Plus(e1, e2)) n = ((eval e1 n) + (eval e2 n)) mod n
  | eval (Minus(e1, e2)) n = ((eval e1 n) - (eval e2 n)) mod n;

val test = Minus(Minus(Plus(Mult(IConst 5, IConst 20), IConst 10), IConst 7), Div(IConst 21, IConst 3));
test;
eval test 90;