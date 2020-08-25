datatype expr =
    IConst of int
    | Div of expr * expr
    | Mult of expr * expr
    | Plus of expr * expr
    | Minus of expr * expr
    | Rect of expr * expr
    | Square of expr
    | RectTriangle of expr * expr * expr
    | Circle of expr
    | RectArea of expr
    | RectPerim of expr
    | SquareArea of expr
    | SquarePerim of expr
    | RectTriangleArea of expr
    | RectTrianglePerim of expr
    | CircleArea of expr
    | CirclePerim of expr;

val pi = 3;

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
  | eval (Minus(e1, e2)) = (eval e1) - (eval e2)
  | eval (RectArea(Rect(e1, e2))) = (eval e1) * (eval e2)
  | eval (RectPerim(Rect(e1, e2))) = 2 * (eval e1) + 2 * (eval e2)
  | eval (SquareArea(Square(e))) =
      let val res = (eval e) in
          res * res
      end
  | eval (SquarePerim(Square(e))) = 4 * (eval e)
  | eval (RectTriangleArea(RectTriangle(e1, e2, e3))) =
      let
          val res1 = (eval e1)
          val res2 = (eval e2)
          val res3 = (eval e3)
      in
          if res1 < res2 andalso res2 < res3 then
              (res2 * res3) div 2
          else if res2 < res1 andalso res2 < res3 then
              (res1 * res3) div 2
          else if res3 < res1 andalso res3 < res2 then
              (res1 * res2) div 2
          else
              raise Match
      end
  | eval (RectTrianglePerim(RectTriangle(e1, e2, e3))) = (eval e1) + (eval e2) + (eval e3)
  | eval (CircleArea(Circle(e))) =
      let val res = (eval e) in
          pi * res * res
      end
  | eval (CirclePerim(Circle(e))) =
      let val res = (eval e) in
          2 * pi * res
      end
  | eval _ = raise Match;

val testRectArea = RectArea(Rect(IConst 2, IConst 3));
testRectArea;
eval(testRectArea);

val testRectPerim = RectPerim(Rect(IConst 2, IConst 3));
testRectPerim;
eval(testRectPerim);

val testSquareArea = SquareArea(Square(IConst 2));
testSquareArea;
eval(testSquareArea);

val testSquarePerim = SquarePerim(Square(IConst 2));
testSquarePerim;
eval(testSquarePerim);

val testRectTriangleArea = RectTriangleArea(RectTriangle(IConst 2, IConst 3, IConst 4));
testRectTriangleArea;
eval(testRectTriangleArea);

val testRectTrianglePerim = RectTrianglePerim(RectTriangle(IConst 2, IConst 3, IConst 4));
testRectTrianglePerim;
eval(testRectTrianglePerim);

val testCircleArea = CircleArea(Circle(IConst 3));
testCircleArea;
eval(testCircleArea);

val testCirclePerim = CirclePerim(Circle(IConst 3));
testCirclePerim;
eval(testCirclePerim);
