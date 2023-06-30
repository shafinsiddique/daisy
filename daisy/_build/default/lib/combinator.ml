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


(* 
A word parser. 
*)
let word_parser word = Parser 
  (fun input -> let parsers = String.fold_left (fun lst char -> (List.cons char lst)) [] word |> List.rev |> List.map (fun chr -> char_parser chr)  in 
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
  
I want a parser that takes in anything and wraps it in a parser. 

handle_char <*> char_parser 'a' 

runParser p1

*)

let fmap f parser = 
  Parser (fun input -> match run_parser parser input with 
                       ParsingSuccess (value, rest) -> ParsingSuccess ((f value), rest)
                      | ParsingError e -> ParsingError e)

let pure value = Parser (fun input -> ParsingSuccess (value, input))

let (<*>) p1 p2 = Parser (fun input -> match run_parser p1 input with 
                            ParsingSuccess (func, rest) -> run_parser (fmap func p2) rest
                            | ParsingError e -> ParsingError e )
