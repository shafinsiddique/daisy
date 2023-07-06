open Combinator

type markdown = Heading of {level: int; text: string} | MarkdownChar of char 
  | Bold of (markdown list) | Italic of (markdown list)

(* 
   
If any of parsers from parser list returns succesful then STOP. 

As long as those don't get matched, run the parser p. store output in lst, keep working. 
*)

let markdown_char_parser = 
  Parser (fun input -> match (run_parser (single_conditional_parser (fun chr -> chr != '\n')) input) with 
    ParsingSuccess (value, rest) -> ParsingSuccess (MarkdownChar value, rest)
    | ParsingError e -> ParsingError e)

  
(* let bold_parser = 
  let handler _ chars _ = 
      (List.fold_left (fun str chr -> str ^ (String.make 1 chr)) "" chars) in pure handler <*> 
  word_parser "**" <*> parse_on_condition (word_parser "**") 
                    (single_conditional_parser (fun chr -> chr != '\n'))  <*> word_parser "**" *)

(*

Internal Parser : bold, link, italacized, 
*)

(* let internal_parser = 
  let parser p handler = 
    pure handler <*> p <*> (parse_on_condition p internal_parser) <*> p  
  and 
    let bold_parser = parser (word_parser "##") (fun _ _ _ -> ()) in  *)
let rec internal_parser () = 
  let bold_parser = get_parser (word_parser "**") (fun _ tokens _ -> Bold tokens)  in 
  let italic_parser = get_parser (word_parser "*") (fun _ tokens _ -> Italic tokens)  in 
    any_of [bold_parser; italic_parser; markdown_char_parser]
and 
get_parser p handler = pure handler <*> p <*> (parse_on_condition_lazy p internal_parser) <*> p 

let heading_parser =  
  let handler tags text = Heading {level=(String.length tags); text=(List.fold_left (fun str chr -> str ^ (String.make 1 chr)) "" text)} in 
  pure handler <*> any_of (List.map word_parser ["######";"#####";"####";"###";"##";"#"]) <*> conditional_parser (fun input -> input != '\n') 