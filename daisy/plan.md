The goal is to create a Static Site Generator that will be able to generate my personal website. 

An example site will have 2 folders: 
    - layouts
    - content


Inside themes, is a directory for each theme. 

- Etch is a theme.

therefore. 

themes
    etch 


Inside each theme folder, there will be the follwing directoris

    - layouts
    - assets
        - css
    - images

Inside layouts is where teh meat of the theme is. 

Right inside layout, any HTML file will be the theme for any file in the main direcotyr. 

So index.html will be here. 

layouts
    - partials (all partial files will be here. if something is referenced as partial, we generate this.)
    _ _default: this direcotry contains template. 
        - single.htmls for all individual md files that don't have a coRrESPINDING HTML FILE.
        - baseof.html s

Here's how the process works. 

If there's a corresponding HTML file for that markdown file. 
GO to that, 
if that hTML file has a DEFINE section then we need to bring in the base. 

Base has a LOOKUP order .

Okay here's how the overall process. 

When Daisy starts, 

It loads files - DIRECTORY BY DIRECTORY from the CONTENT directory. 

At the root, what are the .MD files. 

We start processing those. 

okay parse THE markdown file. 

Now I have a Markdown file with a name content (maybe some metadata?)

We now pass it on to the generator. 

the generator looks at the path where the markdown file came from. 

DOES THE LOOKUP ORDER. 

- Is there a file with thhat name in that directory?
- Yes. Then use that file. 
- Use the genertator structure/algorithm. 
    - If there is a define section, find the parent. 
    - Where could the parent be?
    - That's the lookup order?
    


# New Plan

- Each site has 2 folders : content AND themes 

# Process

- We start with root directory. 

- get markdown file. 

- if index.md in root then it is homepage. 

- to look for its corresponding HTML page, here is the order:
    - index.html in root directory of layouts. 
    - list.html in root directory of layouts. 
    - defaults/index.html
    - defaults/list.html

- Now sections page. 
- go into another directory

    - if _index.md file then 
        : section/list.html in that corresponding direcotry 
        : section/index.html in thhat corresponding directory. 
        : default/list.html
        : default/index.html



daisy build --directory="."
Start reading files from that directory. 
If it is HOME then 
    lookup order : 
        index.html in the corresponding directory. 
        if not
        list.html in corresponding directory 
        if not 
        default.html in default directory 
        if not 
        list.html in default directory 

If if it is regular index.md file then 
       lookup order : 
        index.html in the corresponding directory. 
        if not
        list.html in corresponding directory 
        if not
        list.html in default directory 
        if not 
        default.html in default directory 





Next Steps: 

- Ternary operator : Will help us fix the relative linking issue. 
    - boolean expression ? expression : expression - done.
- Debug mode vs prod mode. - Done 
- copy directories. 
- loop 
- metadata

