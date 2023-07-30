type expression = 
  StringExpression of string | IntExpression of int | BoolExpression of bool  | ErrorExpression of string
  | EmptyExpression | ListExpression of (expression list)