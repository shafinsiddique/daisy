open Markdown
open Combinator
let file_to_markdown path = 
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
    
