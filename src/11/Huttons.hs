-- Chapter Exercises, Page 457

-- 1.

data Expression =
    Literal Integer
  | Addition Expression Expression

evaluate :: Expression -> Integer
evaluate (Literal integer) = integer
evaluate (Addition expression expression') =
  evaluate expression + evaluate expression'

-- 2

printExpression :: Expression -> String
printExpression (Literal integer) = show integer
printExpression (Addition expression expression') =
  printExpression expression ++ " + " ++ printExpression expression'