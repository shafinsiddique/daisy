open Markdown
open Combinator
open Template
open Expression

module StringMap = Map.Make(String)


type markdown_page = MarkdownPage of {page_variables: (expression StringMap.t); site_variables: (expression StringMap.t)} 

let parse_markdown path = 
  let file = 
    try  Some (open_in path) with 
      _ -> None in 
    match file with 
      Some ic -> 
        let file_str = really_input_string ic (in_channel_length ic) in 
          (match run_parser markdown_parser file_str with 
            ParsingError _ -> None 
            | ParsingSuccess (value, _) -> Some value)
      | None -> None 
  
  
let parse_html path = 
  let file = 
    try Some (open_in path) with 
      _ -> None in 
    match file with 
      Some ic -> 
        let file_str = really_input_string ic (in_channel_length ic) in 
          (match (run_parser html_parser file_str) with 
            ParsingSuccess (value, _) -> Some value 
            | ParsingError _ -> None)
        | None -> None 

let markdown_to_html_string markdown = ""
let get_page_variables markdown = 
  let items = [("content", StringExpression (markdown_to_html_string markdown))] in 
    List.fold_left (fun m (key, value) -> StringMap.add key value m) StringMap.empty items
    
let create_markdown_page markdown = 
  let page_variables = get_page_variables markdown in 
    MarkdownPage {page_variables=page_variables; site_variables=StringMap.empty}

(* 
let markdown_to_html path = 
  let md_file = parse_markdown path in 
    match md_file with 
      Some markdown -> 
        Some ()
      | None -> None *)

        
    


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