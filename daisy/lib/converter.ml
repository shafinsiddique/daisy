open Markdown


let get_heading_item_from_level level = 
  match level with 
    1 -> ("<h1>", "</h1>")
    | 2 -> ("<h2>", "</h2>")
    | 3 -> ("<h3>", "</h3>")
    | 4 -> ("<h4>", "</h4>")
    | 5 -> ("<h5>", "</h5>")
    | 6 -> ("<h6>", "</h6>")
    | _ -> ("<h6>", "</h6>")
  
let rec heading_to_string level components = 
  let str = markdown_to_html_string (MarkdownPage components) in 
  let (opening, closing) = get_heading_item_from_level level in 
  Printf.sprintf "%s\n%s\n%s" opening str closing
  
and 

markdown_to_html md = 
    match md with 
      MarkdownString str -> str 
      | MarkdownChar chr -> String.make 1 chr 
      | Heading r -> (heading_to_string r.level r.components)
      | Bold components -> components_to_string "<b>" "</b>" components
      | Italic components -> components_to_string "<i>" "</i>" components
      | Paragraph components -> components_to_string "<p>" "</p>" components
      | Link r -> Printf.sprintf "<a href=\"%s\">\n %s </a>" r.url (markdown_to_html_string (MarkdownPage r.components))

and
components_to_string opening closing components = 
Printf.sprintf "%s\n%s\n%s" opening (markdown_to_html_string (MarkdownPage components)) closing
and 
markdown_to_html_string page = 
  let (MarkdownPage items) = page in 
    List.fold_left (fun str item -> Printf.sprintf "%s%s\n" str item) "" (List.map markdown_to_html items)
