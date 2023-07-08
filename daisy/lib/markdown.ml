open Combinator

type markdown = Heading of {level: int; components: markdown list} | MarkdownChar of char 
  | Bold of (markdown list) | Italic of (markdown list) | Link of {components: markdown list; url: string}
(* 
   
If any of parsers from parser list returns succesful then STOP. 

As long as those don't get matched, run the parser p. store output in lst, keep working. 
*)


let markdown_char_parser = 
  Parser (fun input -> match (run_parser (single_conditional_parser (fun chr -> chr != '\n')) input) with 
    ParsingSuccess (value, rest) -> ParsingSuccess (MarkdownChar value, rest)
    | ParsingError e -> ParsingError e)

let chars_to_string chars = List.fold_left (fun str chr -> str ^ (String.make 1 chr)) "" chars

let rec internal_parser () = 
  let bold_parser = get_parser (word_parser "**") (fun _ tokens _ -> Bold tokens)  in 
  let italic_parser = get_parser (word_parser "*") (fun _ tokens _ -> Italic tokens)  in 
  let link_parser = pure (fun _ components _ _ url _ -> Link {components;url=(chars_to_string url)}) <*> char_parser '[' <*> (parse_on_condition_lazy (any_of [char_parser '\n'; char_parser ']']) internal_parser) <*> char_parser ']' 
            <*> char_parser '(' <*> conditional_parser (fun chr -> chr != ')' && chr != '\n') <*> char_parser ')' in 
    any_of [bold_parser; italic_parser; link_parser; markdown_char_parser]
and 
get_parser p handler= pure handler <*> p <*> (parse_on_condition_lazy (any_of [word_parser "\n\n"; p]) internal_parser) <*> p 

let heading_parser =  
  let handler tags text = Heading {level=(String.length tags); components= text} in 
  pure handler <*> any_of (List.map word_parser ["######";"#####";"####";"###";"##";"#"]) <*> 
  one_or_more (internal_parser ())


(*
What's left? List, Paragraph (Outer level), continous parsing.   

*)