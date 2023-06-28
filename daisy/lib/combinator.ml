type 'a parsing_result = ParsingSuccess of 'a * string | ParsingError of string

type 'a parser = Parser of (string -> 'a parsing_result)

let run_parser parser input = let (Parser f) = parser in f input
(*

The simplest of parsers. Given a character, if it is the current character then return succes. 

*)
let char_parser chr = Parser 
  (fun input -> 
    if String.length input == 0 then ParsingError "No characters present when 1 is expected." 
    else 
      if chr == input.[0] then ParsingSuccess (chr, String.sub input 1 ((String.length input)-1)) else ParsingError "did not find expected char")

(*

A simple combinator parser, keeps trying parsers of a certain type until one of them succeeds.
*)
let rec any_of parsers = Parser 
      (fun input -> 
        match parsers with 
          [] -> ParsingError "Matched none of the parsers"
          | (x::xs) -> match run_parser x input with 
                        ParsingError _ -> (run_parser (any_of xs) input)
                        | result -> result)
  
(* Sometimes we want to run a parser, but if it fails, it's still okay, we don't want to stop
   the sequence of operations.*)
let optional parser = Parser 
  (fun input -> match (run_parser parser input) with 
    ParsingSuccess (value, rest) -> ParsingSuccess (Some value, rest) 
   | ParsingError _ -> ParsingSuccess (None, input))


