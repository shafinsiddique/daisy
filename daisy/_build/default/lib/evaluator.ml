open Template
open Expression
open Content_page
open Combinator

module StringMap = Map.Make(String)


let read_html_page path = 
  let file = 
    try Some (open_in path) with 
      _ -> None in 
    match file with 
      Some ic -> 
        let file_str = really_input_string ic (in_channel_length ic) in 
          (match (run_parser html_parser file_str) with 
            ParsingSuccess (value, _) -> Some (TemplatePage value)
            | ParsingError _ -> None)
        | None -> None 
    
let deconstruct_list_expression expression = match expression with 
  | ListExpression lst -> lst 
  | _ -> []
let get_variable variable_map key = 
  match StringMap.find_opt key variable_map with 
    Some value -> value
    | None -> ErrorExpression (Printf.sprintf "Error in finding variable %s" key)

let set_local_variable name value content_page = 
    let (ContentPage info) = content_page in 
    info.local_variables <- StringMap.add name value (info.local_variables) 
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
  let evaluate_partial_from_path path = match read_html_page path with 
      Some page -> ListExpression (evaluate_template_page page content_page)
      | None -> EmptyExpression in 
    let evaluate_partial expression = match evaluate_node expression content_page with 
        StringExpression path -> evaluate_partial_from_path path
        | _ -> ErrorExpression "Expected string expression, got something else in usepartial." in 
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
    | UsePartial expression -> evaluate_partial expression 
    | Concat strs -> evaluate_concat strs content_page
    | Ternary (condition, option1, option2) -> evaluate_ternary condition option1 option2 content_page
    | LocalVariable name -> (get_variable info.local_variables name)
    | VariableDefinition (name, expression) -> evaluate_variable_def name expression content_page
    | ForLoop (name, condition, body) -> evaluate_for name condition body content_page
    | Metadata _ -> EmptyExpression

and evaluate_concat strs content_page = 
  let exprs = deconstruct_list_expression (evaluate_items strs content_page []) in 
  let rec concat_exprs exprs output_str = 
    match exprs with 
      [] -> Some (output_str)
      | (x::xs) -> match x with 
        StringExpression value -> concat_exprs xs (output_str^value)
        | _ -> None
  in match (concat_exprs exprs "") with 
    Some value -> StringExpression value 
    | None -> ErrorExpression "error concatatenting"

and evaluate_ternary condition option1 option2 content_page= 
  match (evaluate_node condition content_page) with 
    BoolExpression value -> 
        if value then (evaluate_node option1 content_page) else (evaluate_node option2 content_page)
    | _ -> ErrorExpression "Expected boolean expression in ternary expression"
and evaluate_sections sections map content_page = 

  match sections with 
    [] -> map 
    | (x::xs) -> (match x with 
      SectionDef (name, body) -> evaluate_sections xs (StringMap.add name (evaluate_items body content_page []) map) content_page
      | _ -> evaluate_sections xs map content_page)
    
and evaluate_variable_def name expression content_page = 
    let res = evaluate_node expression content_page in 
    let () = set_local_variable name res content_page in
    EmptyExpression

and evaluate_for name condition body content_page = 
  let rec evaluate_from_lst lst output = 
    match lst with 
      [] -> ListExpression (List.rev output)
      | (x::xs) -> 
          let () = set_local_variable name x content_page in 
          let body_evaluated = evaluate_items body content_page [] in 
          evaluate_from_lst xs (List.cons body_evaluated output) in 
  let lst = evaluate_node condition content_page in 
  match lst with 
      ListExpression value -> (evaluate_from_lst value [])
      | _ -> EmptyExpression
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


