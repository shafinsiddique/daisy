open Combinator

type template = TrueExpr | FalseExpr | StringExpr of string | IntExpr of int 
  | SiteVariable of string | PageVariable of string 
  | IfExpr of template * template list| TemplateString of string 
  | UseBase of string | SectionDef of (string * template list) | UsePartial of template 
  | Concat of (template list) | Ternary of (template * template * template)
  | VariableDefinition of (string * template)
  | LocalVariable of string 
  | ForLoop of (string * template * template list)
  | DictionaryIndex of (template * string list)

type template_page = TemplatePage of (template list)

let boolean_parser = 
  Parser (fun input -> match run_parser (any_of [single_word_parser "true"; single_word_parser "false"]) input with 
    ParsingSuccess (value, rest) -> 
      if value = "true" then ParsingSuccess (TrueExpr, rest) else ParsingSuccess (FalseExpr, rest)
    | ParsingError e -> ParsingError e)

let digit_parser = any_of (List.map char_parser ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9';])

let integer_parser = pure (fun sign digits -> 
  let digits = List.fold_left (fun str c -> str ^ String.make 1 c) "" digits in 
  match sign with 
    Some _ -> IntExpr (0-(int_of_string digits))
    | None -> IntExpr (int_of_string digits)) <*> optional (char_parser '-') <*> one_or_more digit_parser

let strings_parser = pure (fun _ word _ -> List.fold_left (fun str chr -> str ^ String.make 1 chr) "" word) <*>  char_parser '"' <*> conditional_parser (fun c -> c != '"') <*> char_parser '"'
let template_strings_parser = Parser (fun input -> 
  match run_parser strings_parser input with 
    ParsingSuccess (str, rest) -> ParsingSuccess ((StringExpr str), rest)
    | ParsingError e -> ParsingError e)
    
let lazy_parser p = Parser (fun input -> run_parser (p ()) input)

let rec create_list current ending lst = 
  if current == ending then List.rev lst 
  else create_list (current+1) ending (List.cons current lst)

let _local_variable_name_parser = 
  let letters = (List.map char_parser (List.map Char.chr (List.append (create_list 97 123 []) (create_list 65 90 [])))) in 
  pure (fun first rest -> (List.fold_left (fun str chr -> str ^ (String.make 1 chr)) "" (List.cons first rest))) <*> (any_of letters) <*> zero_or_more (any_of (List.append letters [char_parser '_'])) 
  
let local_variable_parser = 
  Parser (fun input -> match (run_parser _local_variable_name_parser input) with 
    ParsingSuccess (str, rest) -> ParsingSuccess (LocalVariable str, rest)
    | ParsingError e -> ParsingError e)
  
let chr_to_string c = String.make 1 c
let site_variable_parser = pure (fun _ name ->  SiteVariable name)  <*> word_parser ".Site." <*> _local_variable_name_parser 

let page_variable_parser = pure (fun _ name -> PageVariable name) <*> word_parser ".Page." <*> _local_variable_name_parser

let char_list_to_string lst = List.fold_left (fun str item -> str ^ chr_to_string item) "" lst
let chain_of_indexing_parser = 
  pure (fun _ str _ -> str) <*> char_parser '[' <*> strings_parser <*> char_parser ']'

let handle_template_parsing _ template indexing _ = match indexing with 
  [] -> template
  | lst -> DictionaryIndex (template, lst)

let rec template_expression_parser () = 
  (* let with_brackets p =  in  *)
    pure handle_template_parsing <*> space_and_newline_parser <*> any_of [lazy_parser template_with_brackets; boolean_parser; lazy_parser ternary_parser; lazy_parser variable_def_parser; template_strings_parser; lazy_parser concat_parser; integer_parser; local_variable_parser; site_variable_parser; page_variable_parser;] <*> zero_or_more chain_of_indexing_parser <*> space_and_newline_parser
  
and template_with_brackets () = (pure (fun _ e _ -> e) <*> char_parser '(' <*> template_expression_parser () <*> char_parser ')')


and if_block_parser () = pure (fun _ _ _ _ exprs _ _ -> exprs ) <*> space_and_newline_parser <*>  word_parser "((" <*> space_and_newline_parser <*> single_word_parser "if"
  <*> lazy_parser template_expression_parser <*> word_parser "))" <*> space_and_newline_parser 

and for_block_parser () = pure (fun _ _ name _ expr _ -> (name, expr)) <*> 
word_parser_with_space "((" <*> single_word_parser_with_space "for" <*> _local_variable_name_parser <*> single_word_parser_with_space "in" <*> lazy_parser template_expression_parser <*> word_parser_with_space "))"
(* and for_block_parser () = word_parser_with_space "((" <*> single_word_parser_with_space "for" <*>   *)

and concat_parser () = pure (fun _ exprs -> Concat exprs) <*> single_word_parser "concat" <*> one_or_more (lazy_parser template_expression_parser)  

and ternary_parser () = pure (fun _ condition _ option1 _ option2 -> 
  Ternary (condition, option1, option2)) <*> word_parser "??" <*> (lazy_parser template_expression_parser) <*> 
word_parser "->" <*> (lazy_parser template_expression_parser) <*> word_parser ":" <*> (lazy_parser template_expression_parser) 

and variable_def_parser () = pure (fun name _ expr -> VariableDefinition (name, expr)) <*> _local_variable_name_parser <*> word_parser_with_space ":=" <*> lazy_parser template_expression_parser

let template_char_parser = pure (fun c spaces -> TemplateString ((String.make 1 c) ^ List.fold_left (fun str c -> str ^ (chr_to_string c)) "" spaces)) <*> any_parser <*> space_and_newline_parser

let get_header_parser name = pure (fun _ _ _ str _ _ -> str) <*> word_parser_with_space "((" <*> space_and_newline_parser <*> single_word_parser_with_space name <*> strings_parser  <*> space_and_newline_parser <*> word_parser_with_space "))"

let get_header_with_expression_parser name = pure (fun _ _ _ template _ -> template) <*> word_parser_with_space "((" <*> space_and_newline_parser <*> single_word_parser_with_space name <*> lazy_parser template_expression_parser <*> word_parser_with_space "))"


let use_base_parser = pure (fun template -> UseBase template) <*> get_header_parser "usebase"


let use_partial_parser = pure (fun template -> UsePartial template) <*> get_header_with_expression_parser "usepartial"

(* let section_header_parser = get_header_parser "section" <*>  *)

(* let section_definition_parser =  *)

let ending_declaration_parser word = pure (fun _ _ _-> "") <*> word_parser "((" <*> single_word_parser_with_space word <*> word_parser "))"

let endif_block_parser = ending_declaration_parser "endif"

let endfor_block_parser = ending_declaration_parser "endfor"
let collapse_char str output = 
  match output with 
    [] -> [TemplateString str]
    | (x::xs) -> match x with 
      TemplateString existing -> List.cons (TemplateString (existing ^ str)) xs 
      | _ -> List.cons (TemplateString str) output



let rec collapse lst output =
  match lst with 
    [] -> List.rev output 
    | (x::xs) -> match x with 
      TemplateString str -> collapse xs (collapse_char str output)  
      | _ -> collapse xs (List.cons x output)

let collapse_template template = 
  match template with 
    IfExpr (condition, body) -> IfExpr (condition, collapse body [])
    | SectionDef (name, body) ->  SectionDef (name, collapse body [])
    | _ -> template
let rec template_parser () = 
    let main_parser = pure (fun _ n _ -> n )  <*>  word_parser "((" <*> template_expression_parser () <*> word_parser "))" in 
    pure (fun _ p _ -> p) <*> space_and_newline_parser <*> any_of [main_parser; lazy_parser if_parser; lazy_parser for_parser; lazy_parser section_definition_parser; use_base_parser; use_partial_parser] <*> space_and_newline_parser

and if_parser () = pure (fun exprs body _  -> IfExpr (exprs, collapse body [])) <*> lazy_parser if_block_parser <*> parse_on_condition endif_block_parser
  (any_of [lazy_parser template_parser; template_char_parser]) <*> endif_block_parser

and for_parser () = pure (fun (name, cond) body _ -> ForLoop (name, cond, collapse body [])) <*> lazy_parser for_block_parser <*> parse_on_condition endfor_block_parser (any_of [lazy_parser template_parser; template_char_parser]) <*> endfor_block_parser

and section_definition_parser () = 
 let section_ending_parser = ending_declaration_parser "endsection" in 
  let section_begining_parser = get_header_parser "section" in 
    pure (fun name body _ -> SectionDef (name, collapse body [])) <*> section_begining_parser <*> parse_on_condition section_ending_parser (any_of [lazy_parser template_parser; template_char_parser]) <*> section_ending_parser

let html_parser = 
  Parser 
  (fun input -> match (run_parser (zero_or_more (any_of[lazy_parser template_parser; template_char_parser]) ) input) with 
    ParsingSuccess (value, rest) -> ParsingSuccess (collapse value [], rest)
    | ParsingError e -> ParsingError e)

