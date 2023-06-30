open Combinator

type markdown = Heading of {level: int; text: string}
let heading_parser =  
  let handler tags text = Heading {level=(String.length tags); text=(List.fold_left (fun str chr -> str ^ (String.make 1 chr)) "" text)} in 
  pure handler <*> any_of (List.map word_parser ["######";"#####";"####";"###";"##";"#"]) <*> conditional_parser (fun input -> input != '\n') 