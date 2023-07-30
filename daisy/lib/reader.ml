open Markdown
open Combinator
open Template
open Expression
open Converter
open Content_page
open Evaluator
module StringMap = Map.Make(String)



let parse_markdown path = 
  let file = 
    try  Some (open_in path) with 
      _ -> None in 
    match file with 
      Some ic -> 
        let file_str = really_input_string ic (in_channel_length ic) in 
          (match run_parser markdown_parser file_str with 
            ParsingError _ -> None 
            | ParsingSuccess (value, _) -> Some (MarkdownPage value))
      | None -> None 
  
let parse_markdown2 path = 
  let file = 
    try  Some (open_in path) with 
      _ -> None in 
    match file with 
      Some ic -> 
        let file_str = really_input_string ic (in_channel_length ic) in 
          (match run_parser markdown_parser file_str with 
            ParsingError _ -> None 
            | ParsingSuccess (value, _) -> Some (markdown_to_html_string (MarkdownPage value)))
      | None -> None 
  
  
let parse_html path = 
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

let get_page_variables markdown = 
  let items = [("content", StringExpression (markdown_to_html_string markdown))] in 
    List.fold_left (fun m (key, value) -> StringMap.add key value m) StringMap.empty items
    
let create_markdown_page markdown = 
  let page_variables = get_page_variables markdown in 
    create_content_page page_variables (StringMap.empty) (StringMap.empty)
(* 
let rec template_to_string items content_page output = 
  match items with 
  [] -> (List.fold_left (fun str item -> str ^ item) "" (List.rev output))
  | (x::xs) -> template_node_to_string x 

and template_node_to_string node = match node with 
  TemplateString str -> str 
  | StringExpr str -> str 
  | TrueExpr -> "true"
  | FalseExpr -> "false"
  | IntExpr value -> string_of_int value 
  |  *)
(* let generate_html template_page content_page = 
  let (TemplatePage items) = template_page in 
    template_to_string items content_page [] *)

let markdown_to_html path path2 = 
  let md_file = parse_markdown path in 
    match md_file with 
      Some markdown -> 
        let content_page = create_markdown_page markdown in 
          let html_page = parse_html path2  
        in (match html_page with
            Some template_page -> Some (evaluate_template_page template_page content_page)
            | _ -> None)
      | None -> None

        
    


(*
   
We need a templating engine. 

What does a template engine need. 

it need a concept of boolean ; true or false. 

(( true | false ))

(( "shafin"))

(( 12345 ))

(( .Site.))

(( if isset <variable> ))

{{ endif }}



*)