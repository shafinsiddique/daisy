open Combinator

type template = TrueExpr | FalseExpr 

let boolean_parser = 
  Parser (fun input -> match run_parser (any_of [word_parser "true"; word_parser "false"]) input with 
    ParsingSuccess (value, rest) -> 
      if value = "true" then ParsingSuccess (TrueExpr, rest) else ParsingSuccess (FalseExpr, rest)
    | ParsingError e -> ParsingError e)



let rec template_expression_parser () = 
  let with_brackets p = (pure (fun _ e _ -> e) <*> char_parser '(' <*> p () <*> char_parser ')') in 
    pure (fun _ n _ -> n) <*> space_and_newline_parser <*> any_of [with_brackets template_expression_parser; boolean_parser] <*>  space_and_newline_parser

let template_parser () = 
    pure (fun _ n _ -> n ) <*> word_parser "((" <*> template_expression_parser () <*> word_parser "))"
