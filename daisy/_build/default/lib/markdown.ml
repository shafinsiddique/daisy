open Combinator

type markdown = Heading of {level: int; text: string}

(* 
   
If any of parsers from parser list returns succesful then STOP. 

As long as those don't get matched, run the parser p. store output in lst, keep working. 
*)
let bold_parser = 
  let handler _ chars _ = 
      (List.fold_left (fun str chr -> str ^ (String.make 1 chr)) "" chars) in pure handler <*> 
  word_parser "**" <*> parse_on_condition (word_parser "**") 
                    (single_conditional_parser (fun chr -> chr != '\n'))  <*> word_parser "**"
let heading_parser =  
  let handler tags text = Heading {level=(String.length tags); text=(List.fold_left (fun str chr -> str ^ (String.make 1 chr)) "" text)} in 
  pure handler <*> any_of (List.map word_parser ["######";"#####";"####";"###";"##";"#"]) <*> conditional_parser (fun input -> input != '\n') 