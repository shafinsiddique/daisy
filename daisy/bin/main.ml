open Sys
open Daisy.Content_page
open Daisy.Expression
open Daisy.Converter
open Daisy.Evaluator
open Daisy.Reader
module StringMap = Map.Make(String)

let root = ref ""

let prod = ref false

let site_metadata = ref []
let ends_with str value = 
  let length = String.length value in 
  String.sub str ((String.length str)-length) length = value
let is_markdown file = ends_with file ".md"

let convert_metadata_to_dict items = 
  let dict = StringMap.empty in 
  DictExpression (List.fold_left (fun d (key, value) -> StringMap.add key (StringExpression value) d) dict items) 

let add_metadata path items = 
  match (parse_metadata path) with 
    Some value -> List.cons (("metadata", convert_metadata_to_dict value)) items 
    | None -> items 
let get_page_variables path markdown section_metadata = 

  let items = [("content", StringExpression (markdown_to_html_string markdown)); ("sectionMetadata", section_metadata)] in 
  let items = add_metadata path items in 
    List.fold_left (fun m (key, value) -> StringMap.add key value m) StringMap.empty items 
let get_site_variables root = 
  let items = [("root", StringExpression root); ("prod", BoolExpression !prod)] in 
    List.fold_left (fun m (key, value) -> StringMap.add key value m) StringMap.empty  (List.append items !site_metadata)
let create_content_page_from_markdown path markdown section_metadata = 
  let page_variables = get_page_variables path markdown section_metadata in 
    create_content_page page_variables (get_site_variables !root) (StringMap.empty)

let get_section_path md_file_path =
  let rec join_paths paths str = 
    match paths with 
      [] -> str
      | [x] -> str ^ x
      | (x::xs) -> join_paths xs (str ^ x ^ "/") in 
  let paths = String.split_on_char '/' md_file_path in 
  let rec _iterate_paths paths = 
    match paths with
       [] -> []
       | [_] -> []
       | (x::xs) -> if (x="content") then List.rev (List.tl (List.rev xs)) else _iterate_paths xs in 
    join_paths (_iterate_paths paths) ""

let is_home section_path filename = section_path = "" && filename = "index.md"

let is_section_home filename = filename = "index.md"

let remove_file_extension filename = 
  List.fold_left (fun str item -> str ^ item ) "" (List.rev (List.tl (List.rev (String.split_on_char '.' filename))))

let get_lookup_list md_filename section_path layouts_dir = 
  let base_path = Printf.sprintf "%s/%s" layouts_dir in
  let filename_without_ext = remove_file_extension md_filename in 
  if is_home section_path md_filename 
    then 

      [base_path "index.html"; base_path "list.html"; base_path "defaults/index.html"; base_path "defaults/list.html"] 
  else 
    let child_path = (Printf.sprintf "%s/%s/%s" layouts_dir section_path) in 
    if is_section_home md_filename 

      then 
        [child_path "index.html"; child_path "list.html"; base_path "defaults/list.html"; base_path "defaults/index.html"]
    else 
      [child_path (filename_without_ext ^ ".html"); child_path "single.html"; base_path "defaults/single.html"; base_path "defaults/index.html"] 

let rec get_html_page lookup_list = match lookup_list with 
      [] -> None
      | (x::xs) -> (match parse_html x with 
        Some page -> Some page
        | None -> get_html_page xs)

let list_to_string lst = List.fold_left (fun str item -> str ^ item) "" lst  
(* let list_to_str lst = 
  List.fold_left (fun str item -> str ^ item ^ "\n") "" lst *)
let get_corresponding_html md_file_name section_path layouts_dir = 
  let lookup_list = get_lookup_list md_file_name section_path layouts_dir in 
  let ()=  Printf.printf "%s\n" (list_to_string lookup_list) in  
  get_html_page lookup_list

let get_page_string content_page template_page = 
  strings_of_expressions (evaluate_template_page template_page content_page)

let join_paths paths = 
  let rec _join_paths paths str = 
      match paths with 
        [] -> str
        | [x] -> str ^ x 
        | (x::xs) -> 
          let new_path =  if x = "" then str else (str ^ x ^ "/") in 
            _join_paths xs new_path in 
    _join_paths paths ""
let create_dir name = 
  if file_exists name then () else mkdir name 0o775

let rec create_all_directories current paths = 
  match paths with 
    [] -> ()
    | (x::xs) -> 
      let path = Printf.sprintf "%s/%s" current x in 
      let () = create_dir path in create_all_directories path xs

let write_to_html_file html_str section_path filename  = 
  let paths = List.cons "public" (String.split_on_char '/' section_path) in 
  let () = create_all_directories !root paths in 
  let file_path = join_paths [!root; "public"; section_path; filename] in 
  let oc = open_out file_path in 
  let () = Printf.fprintf oc "%s" html_str in 
  close_out oc


let generate_from_markdown md_file_name md_file_path layouts_dir section_metadata = 
  match (parse_markdown md_file_path) with 
    Some md_page -> 
      let section_path = get_section_path md_file_path in 
        let content_page = create_content_page_from_markdown md_file_path md_page section_metadata in 
          let html_page = get_corresponding_html md_file_name section_path layouts_dir in 
            (match html_page with 
              Some template_page -> 
                let html_str = get_page_string content_page template_page in 
                  let () = write_to_html_file html_str section_path (remove_file_extension md_file_name ^ ".html") in 
                  Printf.printf "Succesfully generated file: %s" (remove_file_extension md_file_name  ^ ".html\n")
              | None -> Printf.printf "found no matching html files ")
    | None -> ()

(* 
Function will return an ExpressionList of ExpressionDictionaries. 
{"title":"Dapr ...",
""`}
*)
let get_section_metadata path files =
  let get_url filepath filename   =  Printf.sprintf "/%s/%s.html" (get_section_path filepath) (remove_file_extension filename)  in 
 
  let rec _get_metadata files output = 
      match files with 
        [] -> ListExpression (List.rev output)
        | (x::xs) -> 
          let file_path = Printf.sprintf "%s/%s" path x in 
          if is_directory file_path then (_get_metadata xs output) else 
          (match (parse_metadata file_path) with 
            Some value -> 
              _get_metadata xs (List.cons (convert_metadata_to_dict 
                (List.cons (("url", (get_url file_path x))) value)) output)
            | None -> _get_metadata xs output)
  in
    _get_metadata files [] 

let rec generate_from_dir content_dir layouts_dir = 
  let content_files = Array.to_list (readdir content_dir) in 
  let section_metadata = get_section_metadata content_dir content_files in 
  let rec process_content_files content_files = 
    match content_files with 
      [] -> ()
      | (x::xs) -> 
        let file_name = Printf.sprintf "%s/%s" content_dir x in 
        if is_directory file_name then 
          let () = generate_from_dir file_name layouts_dir in process_content_files xs 
        else
          if is_markdown file_name then 
            let _ = generate_from_markdown x file_name layouts_dir section_metadata in process_content_files xs  
          else 
            process_content_files xs in 
  process_content_files content_files 

  
let set_site_metadata content_dir = 
  let metadata_file = Printf.sprintf "%s/%s" content_dir "metadata.daisy" in 
  if file_exists metadata_file then 
      match parse_metadata metadata_file with 
        Some value -> 
          site_metadata := List.map (fun (k, v) -> (k, StringExpression v)) value
        | None -> ()
  else ()
let build_site () = 
  let content_dir = (Printf.sprintf "%s/content" !root) in 
  let layouts_dir = (Printf.sprintf "%s/layouts" !root) in 
  if file_exists content_dir then 
    (if file_exists layouts_dir then (
      let () = set_site_metadata content_dir in
      generate_from_dir content_dir layouts_dir) else (Printf.printf "No layout directory"))
    else (Printf.printf "No content directory")
  
let () =
  let () = root := "./sample_site" in 
  build_site ()
