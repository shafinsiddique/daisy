open Combinator

type list_type = UpperLetter | Number | LowerLetter

type markdown = Heading of {level: int; components: markdown list} | MarkdownChar of char 
  | Bold of (markdown list) | Italic of (markdown list) | Link of {components: markdown list; url: string}
  | Paragraph of (markdown list) | MarkdownString of string
  | UnorderedList of (markdown list) list
  | Metadata of (string * string) list
  | OrderedList of {numbering_type: list_type; start: int; items: (markdown list) list}

type markdown_page = MarkdownPage of (markdown list)

let space_and_newline_parser = zero_or_more (any_of [char_parser ' '; char_parser '\n'])

let markdown_char_parser = 
  Parser (fun input -> match (run_parser (single_conditional_parser (fun chr -> chr != '\n')) input) with 
    ParsingSuccess (value, rest) -> ParsingSuccess (MarkdownChar value, rest)
    | ParsingError e -> ParsingError e)

let chars_to_string chars = List.fold_left (fun str chr -> str ^ (String.make 1 chr)) "" chars
let strings_parser = pure (fun _ word _ -> List.fold_left (fun str chr -> str ^ String.make 1 chr) "" word) <*>  char_parser '"' <*> conditional_parser (fun c -> c != '"') <*> char_parser '"'
let rec internal_parser () = 
  let bold_parser = get_parser (word_parser "**") (fun _ tokens _ -> Bold tokens)  in 
  let italic_parser = get_parser (word_parser "*") (fun _ tokens _ -> Italic tokens)  in 
  let link_parser = pure (fun _ components _ _ url _ -> Link {components;url=(chars_to_string url)}) <*> char_parser '[' <*> (parse_on_condition_lazy (any_of [word_parser "\n\n"; word_parser "\n "; word_parser "]"]) internal_parser) <*> char_parser ']' 
            <*> char_parser '(' <*> conditional_parser (fun chr -> chr != ')' && chr != '\n') <*> char_parser ')' in 
    any_of [bold_parser; italic_parser; link_parser; markdown_char_parser]
and 
get_parser p handler= pure handler <*> p <*> (parse_on_condition_lazy (any_of [word_parser "\n"; p]) internal_parser) <*> p 


let rec collapse_chars node = 
  
  match node with 
    Bold c -> Bold (collapse c [])
    | Italic c -> Italic (collapse c [])
    | Paragraph c -> Paragraph (collapse c [])
    | Link {components;url;} -> Link {components=(collapse components []); url=url}
    | Heading {components; level;} -> Heading {components=(collapse components []); level=level}
    | UnorderedList items -> UnorderedList (List.map (fun item -> collapse item []) items)
    | OrderedList {numbering_type; start; items} -> OrderedList {numbering_type; start; items = (List.map (fun item -> collapse item []) items)}
    | node -> node 

and join_or_add char output = 
    let new_str = String.make 1 char in 
      match output with 
        [] -> [MarkdownString new_str]
        | (x::xs) -> (match x with 
          MarkdownString str -> List.cons (MarkdownString (str ^ new_str)) xs
          | _ -> List.cons (MarkdownString new_str) output)  
  
and  collapse components output = 
      match components with 
        [] -> List.rev output
        | (x::xs) -> 
          match x with 
            MarkdownChar c -> collapse xs (join_or_add c output)
            | item -> collapse xs (List.cons (collapse_chars item) output)  
let heading_parser =  
      let handler tags text = collapse_chars(Heading {level=(String.length tags); components=text}) in 
      pure handler <*> any_of (List.map word_parser ["######";"#####";"####";"###";"##";"#"]) <*> 
      one_or_more (internal_parser ()) 

let unordered_list_item_parser = pure (fun _ _ items _ _ -> items) <*> char_parser '-' <*> one_or_more (char_parser ' ') <*> zero_or_more (internal_parser ()) <*> space_parser <*> optional (char_parser '\n')

let unordered_list_parser = pure (fun items -> collapse_chars (UnorderedList items)) <*> 
  one_or_more unordered_list_item_parser

let numbered_list_item = pure (fun num _ _ items _ _ -> (num, items)) <*> integer_parser <*> char_parser '.' <*> one_or_more (char_parser ' ') <*> zero_or_more (internal_parser ()) <*> space_parser <*> optional (char_parser '\n')

let get_starting items = match items with 
  [] -> 1
  | ((x, _)::_) -> x

let rec create_list current ending lst = 
    if current == ending then List.rev lst 
    else create_list (current+1) ending (List.cons current lst)
let numbered_list_parser = 
  let get_only_items items = List.map (fun (_, item) -> item) items in 
  pure (fun items -> OrderedList {numbering_type=Number; start=get_starting items; items=(get_only_items items)}) <*> one_or_more numbered_list_item

let alphabeticized_list_parser chars = 
  let chars_parser = any_of (List.map char_parser chars) in 
  pure (fun _ _ _ items _ _ -> items) <*> chars_parser <*> char_parser '.' <*> (one_or_more (char_parser ' ')) <*> zero_or_more (internal_parser ())
  <*> space_parser <*> optional (char_parser '\n')

let uppercase_list_parser = 
  let upper_cases = (List.map (Char.chr) (create_list 65 90 [])) in 
  pure (fun items -> OrderedList {numbering_type=UpperLetter; start=0; items=items;}) <*> (one_or_more (alphabeticized_list_parser upper_cases))

let lowercase_list_parser =
  let lower_cases = (List.map (Char.chr) (create_list 97 123 [])) in 
  pure (fun items -> OrderedList {numbering_type=LowerLetter; start=0; items=items;}) <*> (one_or_more (alphabeticized_list_parser lower_cases))

let all_alphabeticized_list_parser = 
  any_of [uppercase_list_parser; lowercase_list_parser]
  
let ordered_list_parser = pure (fun e -> collapse_chars e) <*> any_of [numbered_list_parser; all_alphabeticized_list_parser]
let metadata_heading_parser = pure (fun _ _ -> "") <*> word_parser "---" <*> ends_with_newline_parser

let metadata_item_parser = pure (fun key _ _ value _ -> (key, value)) <*> strings_parser <*> space_parser <*> word_parser ":" <*> strings_parser <*> ends_with_newline_parser

let metadata_ending_parser = pure (fun _ _ -> "") <*> word_parser "---" <*> ends_with_newline_parser

let _metadata_parser = pure (fun _ items _ -> items) 
  <*> metadata_heading_parser 
  <*> one_or_more metadata_item_parser 
  <*> metadata_ending_parser

let metadata_parser = pure (fun _ items -> items) <*> space_and_newline_parser <*> _metadata_parser

let md_metadata_parser = pure (fun item -> Metadata item) <*> metadata_parser
let paragraph_parser = pure (fun items -> collapse_chars(Paragraph items)) 
  <*> one_or_more (internal_parser ())

(*

Only time markdown parser would fail is if string is empty. 
*)
let markdown_parser =  
  one_or_more (pure (fun _ p  _-> p ) <*> space_and_newline_parser <*> any_of [heading_parser; md_metadata_parser; unordered_list_parser; ordered_list_parser; paragraph_parser] <*> space_and_newline_parser ) 
(*
What's left? List, Paragraph (Outer level), continous parsing.   

*)