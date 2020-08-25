datatype bexpr =
    BConst of bool
    | And of bexpr * bexpr
    | Or of bexpr * bexpr;

fun evalb (BConst b) = b
  | evalb (And(b1, b2)) = (evalb b1) andalso (evalb b2)
  | evalb (Or(b1, b2)) = (evalb b1) orelse (evalb b2);

datatype expr =
    IConst of int
    | Div of expr * expr
    | Mult of expr * expr
    | Plus of expr * expr
    | Minus of expr * expr
    | Var of string;

val state = [
    ("a", 10),
    ("b", 20),
    ("c", 30),
    ("d", 40),
    ("e", 50),
    ("f", 60),
    ("g", 70),
    ("h", 80),
    ("i", 90),
    ("j", 100)
];

fun lookup [] id = raise Match
  | lookup ((k:string, v)::t) id = if k = id then v else lookup t id;

fun eval (Var x) = lookup state x
  | eval (IConst i) = i
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

val test1 = Minus(Plus(Var "b", Var "c"), Var "a");
test1;
eval test1;

val test2 = And(Or(BConst true, BConst false), BConst true);
test2;
evalb test2;