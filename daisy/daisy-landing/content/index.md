
# Welcome to Daisy

[Daisy](https://github.com/shafinsiddique/daisy) is a static site generator that I built for my own needs. It is ridiculously simple and minimal. This website is actually generated using Daisy. So is my own [personal website.](https://shafin.me) 

# Quick Start 

1. Clone Git Repository
--code
> git clone https://github.com/shafinsiddique/daisy.git
--endcode
2. Build sample site
--code
> dune exec -- daisy --root "./daisy-landing"
--endcode

# About Daisy

Daisy is built using [OCaml](https://ocaml.org) and is heavily inspired by Hugo. It uses a custom markdown parser and a custom templating engine. 

It is extremely fast and made for people who are in need of a static site generator but don't want to spend too much time reading documentation. 

# How Templates Work

A Daisy template is a directory with two sub directories : layouts and content. The layouts directory contains all the HTML template files and the content directory contains all the markdown files. 

Daisy goes through all files in the content directory and generates the corresponding static file for each. There is a lookup order depending on the type of file it is. 

If you want to access the markdown content in the HTML template, you can do so using the page variable 'content'. Page variables can be accessed using the following syntax : ".Page.content". 


# Daisy's Templating 

Daisy's templating syntax is inspired by based on Lisp. Here are some examples:

- Import a partial template file 

--code
(( usepartial (concat (.Site.root) "/layouts/partials/head.html") ))
--endcode

- Reference the Markdown file's content
--code
(( .Page.content ))
--endcode

- Create a local variable
--code
(( partialHeadUrl := (concat (.Site.root) "/layouts/partials/head.html")))
--endcode

- Ternary Operator

--code
(( baseUrl := ?? .Site.prod -> .Site.baseUrl : "emptyUrl" ))
--endcode


