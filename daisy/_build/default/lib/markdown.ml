open Combinator

type markdown = Heading of {level: int; components: markdown list} | MarkdownChar of char 
  | Bold of (markdown list) | Italic of (markdown list) | Link of {components: markdown list; url: string}
  | Paragraph of (markdown list) | MarkdownString of string

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
get_parser p handler= pure handler <*> p <*> (parse_on_condition_lazy p internal_parser) <*> p 




let rec collapse_chars node = 
  let join_or_add char output = 
    let new_str = String.make 1 char in 
    match output with 
      [] -> [MarkdownString new_str]
      | (x::xs) -> (match x with 
        MarkdownString str -> List.cons (MarkdownString (str ^ new_str)) xs
        | _ -> List.cons (MarkdownString new_str) output) in 

  let rec collapse components output = 
    match components with 
      [] -> List.rev output
      | (x::xs) -> 
        match x with 
          MarkdownChar c -> collapse xs (join_or_add c output)
          | item -> collapse xs (List.cons (collapse_chars item) output) in 
    
  match node with 
    Bold c -> Bold (collapse c [])
    | Italic c -> Italic (collapse c [])
    | Paragraph c -> Paragraph (collapse c [])
    | Link {components;url;} -> Link {components=(collapse components []); url=url}
    | Heading {components; level;} -> Heading {components=(collapse components []); level=level}
    | node -> node 

let heading_parser =  
      let handler tags text = collapse_chars(Heading {level=(String.length tags); components=text}) in 
      pure handler <*> any_of (List.map word_parser ["######";"#####";"####";"###";"##";"#"]) <*> 
      one_or_more (internal_parser ())
let paragraph_parser = pure (fun items -> collapse_chars(Paragraph items)) 
  <*> one_or_more (internal_parser ())

(*

Only time markdown parser would fail is if string is empty. 
*)
let markdown_parser =  any_of [heading_parser; paragraph_parser]
(*
What's left? List, Paragraph (Outer level), continous parsing.   

*)