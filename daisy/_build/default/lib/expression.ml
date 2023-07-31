type expression = 
  StringExpression of string | IntExpression of int | BoolExpression of bool  | ErrorExpression of string
  | EmptyExpression | ListExpression of (expression list)

let rec string_of_expression expr = match expr with 
  StringExpression str -> str
  | IntExpression value -> string_of_int value 
  | BoolExpression value -> string_of_bool value
  | ErrorExpression value -> Printf.sprintf "Error: %s" value 
  | EmptyExpression -> ""
  | ListExpression exprs -> List.fold_left (fun str item -> str ^ item) "" (List.map string_of_expression exprs) 

let strings_of_expressions exprs = List.fold_left (fun str item -> str ^ item) "" (List.map string_of_expression exprs)