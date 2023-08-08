
module StringMap = Map.Make(String)

type expression = 
  StringExpression of string | IntExpression of int | BoolExpression of bool  | ErrorExpression of string
  | EmptyExpression | ListExpression of (expression list) | DictExpression of (expression StringMap.t)

let rec string_of_expression expr = match expr with 
  StringExpression str -> str
  | IntExpression value -> string_of_int value 
  | BoolExpression value -> string_of_bool value
  | ErrorExpression value -> Printf.sprintf "Error: %s" value 
  | EmptyExpression -> ""
  | ListExpression exprs -> List.fold_left (fun str item -> str ^ item) "" (List.map string_of_expression 
  exprs)
  | DictExpression items -> StringMap.fold (fun key value accum -> accum ^ (Printf.sprintf "(%s, %s)\n" key (string_of_expression value)) ) items ""

let strings_of_expressions exprs = List.fold_left (fun str item -> str ^ item ^ "\n") "" (List.map string_of_expression exprs)