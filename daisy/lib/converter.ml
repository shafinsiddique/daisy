open Markdown
open Combinator
let file_to_markdown path = 
  let ic = open_in path in 
    let rec to_markdown items = 
      let parsed = try (run_parser markdown_parser (input_line ic)) with
        End_of_file -> ParsingError "End of file" 
      in match parsed with 
        ParsingSuccess (value, _) -> to_markdown (List.cons value items) 
        | ParsingError "End of file" -> List.rev items 
        | _ -> to_markdown items in 
  to_markdown []
