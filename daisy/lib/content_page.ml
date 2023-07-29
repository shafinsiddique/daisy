open Expression
module StringMap = Map.Make(String)

type content_page = ContentPage of {page_variables: (expression StringMap.t); site_variables: (expression StringMap.t)} 
