open Template
open Expression
open Content_page

module StringMap = Map.Make(String)

let read_html_page _ = 
    Some (TemplatePage [])
let deconstruct_list_expression expression = match expression with 
  | ListExpression lst -> lst 
  | _ -> []
let get_variable variable_map key = 
  match StringMap.find_opt key variable_map with 
    Some value -> value
    | None -> ErrorExpression

let rec evaluate_items items content_page output =
  match items with 
    [] -> ListExpression (List.rev output)
    | (x::xs) -> evaluate_items xs content_page (List.cons (evaluate_node x content_page) output)
and evaluate_node node content_page = 
  let (ContentPage info) = content_page in 
  let evaluate_if condition body =  match (evaluate_node condition content_page ) with 
      BoolExpression value -> 
            (match value with 
              true -> evaluate_items body content_page []
              | false -> EmptyExpression)
      | _ -> EmptyExpression in
  let evaluate_partial path = match read_html_page path with 
      Some page -> ListExpression (evaluate_template_page page content_page)
      | None -> EmptyExpression in 
  match node with 
    TemplateString str -> StringExpression str
    | IntExpr value -> IntExpression value
    | TrueExpr -> BoolExpression true
    | FalseExpr -> BoolExpression false
    | StringExpr str -> StringExpression str
    | SiteVariable name -> (get_variable info.site_variables name)
    | PageVariable name -> (get_variable info.page_variables name)
    | UseBase _ -> EmptyExpression 
    | IfExpr (condition, body) -> evaluate_if condition body 
    | SectionDef _ -> EmptyExpression
    | UsePartial path -> evaluate_partial path 
and evaluate_sections sections map content_page = 

  match sections with 
    [] -> map 
    | (x::xs) -> (match x with 
      SectionDef (name, body) -> evaluate_sections xs (StringMap.add name (evaluate_items body content_page []) map) content_page
      | _ -> evaluate_sections xs map content_page)
    
and get_sections items content_page = 
  evaluate_sections (List.filter (fun node -> match node with 
    | SectionDef _ -> true
    | _ -> false) items) StringMap.empty content_page


and evaluate_page_using_base path items content_page = 
  let (ContentPage info) = content_page in 
  match (read_html_page path) with 
    Some page -> evaluate_template_page page (create_content_page info.page_variables info.site_variables 
    (get_sections items content_page))
    | None -> []

and evaluate_template_page page content_page = 
  let (TemplatePage items) = page in 
      match items with 
        [] -> []
        | (x::_) -> (match x with 
          UseBase path -> evaluate_page_using_base path items content_page 
          | _ -> deconstruct_list_expression (evaluate_items items content_page []))


