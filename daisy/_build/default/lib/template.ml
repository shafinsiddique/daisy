open Combinator

type template = TrueExpr | FalseExpr 

let boolean_parser = 
  Parser (fun input -> match run_parser (any_of [word_parser "true"; word_parser "false"]) input with 
    ParsingSuccess (value, rest) -> 
      if value = "true" then ParsingSuccess (TrueExpr, rest) else ParsingSuccess (FalseExpr, rest)
    | ParsingError e -> ParsingError e)

    
let lazy_parser p = Parser (fun input -> run_parser (p ()) input)

let rec template_expression_parser () = 
  (* let with_brackets p =  in  *)
    pure (fun _ n _ -> n) <*> space_and_newline_parser <*> any_of [lazy_parser template_with_brackets; boolean_parser] <*>  space_and_newline_parser
and template_with_brackets () = (pure (fun _ e _ -> e) <*> char_parser '(' <*> template_expression_parser () <*> char_parser ')')

let template_parser () = 
    pure (fun _ n _ -> n ) <*> word_parser "((" <*> template_expression_parser () <*> word_parser "))"
