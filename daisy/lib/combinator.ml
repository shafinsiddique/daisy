type 'a parsing_result = ParsingSuccess of 'a * string | ParsingError of string

type 'a parser = Parser of (string -> 'a parsing_result)

let run_parser parser input = let (Parser f) = parser in f input

let rec create_list current length lst = if current >= length 
    then lst else create_list (current+1) length (List.cons current lst)
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


(* 
Takes in a series of parsers, only returns parsing success if ALL of them return succesfully. 
Returns a list of all the values on parsing success
*)

let sequence parsers = 
  let rec _sequence parsers output = 
    Parser (fun input -> match parsers with 
    [] -> ParsingSuccess (List.rev output, input)
    | (x::xs) -> match (run_parser x input) with 
              ParsingSuccess (value, rest) -> run_parser (_sequence xs (List.cons value output)) rest
              | ParsingError e -> ParsingError e) 
    in _sequence parsers []


let empty_string_parser = Parser (fun input -> 
  if input = "" then ParsingSuccess (' ',"") else ParsingError "Expected empty string")

let end_of_word_parser = any_of [char_parser ' '; char_parser '\n'; empty_string_parser]

(* 
A word parser. 
*)
let word_parser word = Parser 
  (fun input -> let parsers = (String.fold_left (fun lst char -> (List.cons char lst)) [] word |> List.rev |> List.map (fun chr -> char_parser chr)) in 
    let result = run_parser (sequence parsers) input in 
      match result with 
        ParsingSuccess (value, rest) -> ParsingSuccess (List.fold_left (fun str chr -> str ^ (String.make 1 chr)) "" value, rest)
        | ParsingError e -> ParsingError e)

(*
A parser if there's a digit, returns the digit, otherwise returns None.
*)
let digit_parser = any_of (List.map 
(fun num -> let chr = match num with 
  0 -> '0'
  | 1 -> '1'
  | 2 -> '2'
  | 3 -> '3'
  | 4 -> '4'
  | 5 -> '5'
  | 6 -> '6'
  | 7 -> '7'
  | 8 -> '8'
  | 9 -> '9'
  | _ -> 'h' in char_parser chr) (create_list 0 10 []))


(* 
Keep parsing for as long as the conidtion is ture.    
*)
let conditional_parser f = 
  let rec _conditional_parser input index output = 
    if index >= String.length input then (List.rev output, index) else 
    let current_char = input.[index] in 
      if (f current_char) then 
        _conditional_parser input (index+1) (List.cons current_char output)
      else (List.rev output, index) in 
  Parser (fun input -> 
    let (output, index) = _conditional_parser input 0 [] in 
      ParsingSuccess (output, String.sub input index ((String.length input)-index)))

(*
  
I want a parser that takes in anything and wraps it in a parser. 

handle_char <*> char_parser 'a' 

runParser p1

*)

let fmap f parser = 
  Parser (fun input -> match run_parser parser input with 
                       ParsingSuccess (value, rest) -> ParsingSuccess ((f value), rest)
                      | ParsingError e -> ParsingError e)

let pure value = Parser (fun input -> ParsingSuccess (value, input))

(* 
Applicative style parsing.    
*)
let (<*>) p1 p2 = Parser (fun input -> match run_parser p1 input with 
                            ParsingSuccess (func, rest) -> run_parser (fmap func p2) rest
                            | ParsingError e -> ParsingError e )

let zero_or_more parser = 
  let rec _zero_or_more input output = match (run_parser parser input) with 
    ParsingSuccess (value, rest) -> _zero_or_more rest (List.cons value output)
    | ParsingError _ -> ParsingSuccess (List.rev output, input)
  in Parser (fun input -> _zero_or_more input [])

let one_or_more parser =
  Parser (fun input -> match run_parser (zero_or_more parser) input with 
    ParsingSuccess (value, rest) -> (match value with 
      [] -> ParsingError "Expected one matched item, found none."
      | lst -> ParsingSuccess (lst, rest))
    | ParsingError e -> ParsingError e)

let parse_on_condition condition parser = 
  let rec _parse_on_condition input output = 
    match run_parser condition input with 
      ParsingSuccess (_, _) -> ParsingSuccess ((List.rev output), input)
      | ParsingError _ -> 
        (match run_parser parser input with 
          ParsingSuccess (value, rest) -> _parse_on_condition rest (List.cons value output)
          | ParsingError _ -> ParsingSuccess ((List.rev output), input)) 
  in Parser (fun input -> _parse_on_condition input [])

(* 

The follwoing 2 are parsers work in this way :

- If the condition parser succeeds, end immediately as a success. 

- Otherwise run the parser. 

Lazy and Non lazy versions available




*)
let parse_on_condition_lazy condition parser = 
    let rec _parse_on_condition input output = 
      match run_parser condition input with 
        ParsingSuccess (_, _) -> ParsingSuccess ((List.rev output), input)
        | ParsingError _ -> 
          (match run_parser (parser ()) input with 
            ParsingSuccess (value, rest) -> _parse_on_condition rest (List.cons value output)
            | ParsingError _ -> ParsingSuccess ((List.rev output), input)) 
    in Parser (fun input -> _parse_on_condition input [])

let space_and_newline_parser = zero_or_more (any_of [char_parser ' '; char_parser '\n'])
let single_conditional_parser condition =
  Parser (fun input -> if (String.length input == 0) 
                          then ParsingError "No chars found" 
                          else 
                            let current_char = input.[0] in 
                              if condition current_char then 
                                ParsingSuccess (current_char, String.sub input 1 ((String.length input)-1))
                                else (ParsingError "condition failed"))

let any_parser =  Parser (fun input -> 
  if String.length input > 0 then ParsingSuccess (input.[0], (String.sub input 1 ((String.length input)-1))) 
  else ParsingError "No chars left to parse")

let single_word_parser word = pure (fun word _ -> word) <*> word_parser word <*> end_of_word_parser

let single_word_parser_with_space word = pure (fun _ word _ _ -> word) <*> space_and_newline_parser <*> word_parser word <*> end_of_word_parser <*> space_and_newline_parser 

let word_parser_with_space word = pure (fun _ word _ -> word) <*> space_and_newline_parser <*> word_parser word <*> space_and_newline_parser