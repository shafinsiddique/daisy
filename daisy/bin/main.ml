open Sys
open Daisy.Reader
let ends_with str value = 
  let length = String.length value in 
  String.sub str ((String.length str)-length) length = value
let is_markdown file = ends_with file ".md"

let generate_from_markdown md_file layouts_dir = 
  match (parse_markdown md_file) with 
    Some md_page -> ()
    | None -> ()
let rec generate_from_dir content_dir layouts_dir = 
  let process_content_files content_files = 
    match content_files with 
      [] -> ()
      | (x::xs) -> 
        if is_directory x then generate_from_dir x (Printf.sprintf "%s/%s" layouts_dir x) else
          if is_markdown x then 
            generate_from_markdown x layouts_dir in 
  let content_files = readdir content_dir in 
  process_content_files (Array.to_list content_files)  
let build_from_dir path = 
  if file_exists (Printf.sprintf "%s/content" path) then 
    (if file_exists (Printf.sprintf "%s/layouts" path) then () else (Printf.printf "No content directory"))
    else (Printf.printf "No content directory")
  
let () = print_endline "Hello, World!"
