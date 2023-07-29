open Template
open Expression
open Content_page
module StringMap = Map.Make(String)
let get_variable variable_map key = 
  match StringMap.find variable_map key with 
    Some value -> value
    | None -> ErrorExpression

let rec evaluate_items items content_page output =
  let (ContentPage r) = content_page in 
  match items with 
    [] -> List.rev output
    | (x::xs) -> evaluate_items xs content_page (List.cons (evaluate_node x r.site_variables) output)
and evaluate_node node site_vars page_vars = 
  match node with 
    TemplateString str -> StringExpression str
    | IntExpr value -> IntExpression value
    | TrueExpr -> BoolExpression true
    | FalseExpr -> BoolExpression false
    | StringExpr str -> StringExpression str
    | SiteVariable name -> (get_variable site_vars name)
    | 
let evaluate_template_page page content_page= 
  let (TemplatePage items) = page in 
    evaluate_items items