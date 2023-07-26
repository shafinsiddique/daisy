open Markdown
open Combinator
open Template
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