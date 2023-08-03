open Expression
module StringMap = Map.Make(String)

type content_page = ContentPage of {page_variables: (expression StringMap.t); site_variables: (expression StringMap.t); sections: (expression  StringMap.t); mutable local_variables: (expression StringMap.t)} 

let create_content_page page_variables site_variables sections = 
  ContentPage {site_variables; page_variables; sections; local_variables = StringMap.empty}