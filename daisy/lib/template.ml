open Combinator

type template = TrueExpr | FalseExpr | StringExpr of string | IntExpr of int
let boolean_parser = 
  Parser (fun input -> match run_parser (any_of [word_parser "true"; word_parser "false"]) input with 
    ParsingSuccess (value, rest) -> 
      if value = "true" then ParsingSuccess (TrueExpr, rest) else ParsingSuccess (FalseExpr, rest)
    | ParsingError e -> ParsingError e)

let digit_parser = any_of (List.map char_parser ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9';])


let integer_parser = pure (fun sign digits -> 
  let digits = List.fold_left (fun str c -> str ^ String.make 1 c) "" digits in 
  match sign with 
    Some _ -> IntExpr (0-(int_of_string digits))
    | None -> IntExpr (int_of_string digits)) <*> optional (char_parser '-') <*> one_or_more digit_parser

let strings_parser = Parser (fun input -> 
  match run_parser (pure (fun _ word _ -> List.fold_left (fun str chr -> str ^ String.make 1 chr) "" word) <*>  char_parser '"' <*> conditional_parser (fun c -> c != '"') <*> char_parser '"') input with 
    ParsingSuccess (str, rest) -> ParsingSuccess ((StringExpr str), rest)
    | ParsingError e -> ParsingError e)
    
let lazy_parser p = Parser (fun input -> run_parser (p ()) input)

let rec template_expression_parser () = 
  (* let with_brackets p =  in  *)
    pure (fun _ n _ -> n) <*> space_and_newline_parser <*> any_of [lazy_parser template_with_brackets; boolean_parser; strings_parser; integer_parser] <*>  space_and_newline_parser
and template_with_brackets () = (pure (fun _ e _ -> e) <*> char_parser '(' <*> template_expression_parser () <*> char_parser ')')

let template_parser () = 
    pure (fun _ n _ -> n ) <*> word_parser "((" <*> template_expression_parser () <*> word_parser "))"
