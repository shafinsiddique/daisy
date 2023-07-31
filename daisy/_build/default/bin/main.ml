open Sys
open Daisy.Content_page
open Daisy.Expression
open Daisy.Converter
open Daisy.Evaluator
open Daisy.Reader
let ends_with str value = 
  let length = String.length value in 
  String.sub str ((String.length str)-length) length = value
let is_markdown file = ends_with file ".md"



let get_page_variables markdown = 
  let items = [("content", StringExpression (markdown_to_html_string markdown))] in 
    List.fold_left (fun m (key, value) -> StringMap.add key value m) StringMap.empty items

let create_content_page_from_markdown markdown = 
  let page_variables = get_page_variables markdown in 
    create_content_page page_variables (StringMap.empty) (StringMap.empty)

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
    then [base_path "index.html"; base_path "list.html"; base_path "defaults/index.html"; base_path "defaults/list.html"] 
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

(* let list_to_str lst = 
  List.fold_left (fun str item -> str ^ item ^ "\n") "" lst *)
let get_corresponding_html md_file_name section_path layouts_dir = 
  let lookup_list = get_lookup_list md_file_name section_path layouts_dir in 
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
let write_to_html_file file_path html_str = 
  let oc = open_out file_path in 
  let () = Printf.fprintf oc "%s" html_str in 
  close_out oc

let create_dir name = 
  if file_exists name then () else mkdir name 0o775
let generate_from_markdown root md_file_name md_file_path layouts_dir = 
  match (parse_markdown md_file_path) with 
    Some md_page -> 
      let section_path = get_section_path md_file_path in 
        let content_page = create_content_page_from_markdown md_page in
          let html_page = get_corresponding_html md_file_name section_path layouts_dir in 
            (match html_page with 
              Some template_page -> 
                let html_str = get_page_string content_page template_page in 
                  let () = create_dir (join_paths [root; "public"]) in 
                  let () = write_to_html_file (join_paths [root; "public"; section_path; remove_file_extension md_file_name ^ ".html"]) html_str in 
                  Printf.printf "Succesfully generated file %s" (remove_file_extension md_file_name  ^ ".html\n")
              | None -> Printf.printf "found no matching html files ")
    | None -> ()
let rec generate_from_dir root content_dir layouts_dir = 
  let rec process_content_files content_files = 
    match content_files with 
      [] -> ()
      | (x::xs) -> let file_name = Printf.sprintf "%s/%s" content_dir x in 
        if is_directory file_name then 
          let () = generate_from_dir root file_name layouts_dir in process_content_files xs 
        else
          if is_markdown file_name then 
            let _ = generate_from_markdown root x file_name layouts_dir in process_content_files xs in 
  let content_files = readdir content_dir in 
  process_content_files (Array.to_list content_files)  
let build_from_dir path = 
  let content_dir = (Printf.sprintf "%s/content" path) in 
  let layouts_dir = (Printf.sprintf "%s/layouts" path) in 
  if file_exists content_dir then 
    (if file_exists layouts_dir then (generate_from_dir path content_dir layouts_dir) else (Printf.printf "No layout directory"))
    else (Printf.printf "No content directory")
  
let () = build_from_dir "./sample_site"
