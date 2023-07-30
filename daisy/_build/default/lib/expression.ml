type expression = 
  StringExpression of string | IntExpression of int | BoolExpression of bool  | ErrorExpression
  | EmptyExpression | ListExpression of (expression list)