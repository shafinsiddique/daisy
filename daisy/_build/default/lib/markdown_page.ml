open Expression

module StringMap = Map.Make(String)


type markdown_page = PageVariables of (expression StringMap.t) | SiteVariables of (expression StringMap.t) 

