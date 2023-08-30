
# Welcome to Daisy

[Daisy](https://github.com/shafinsiddique/daisy) is a static site generator that I built for my own needs. It is ridiculously simple and minimal. You can find more details about Daisy on it's [documentation site](https://shafinsiddique.github.io/daisy-docs/). Fun fact : The documentation site was actually generated using Daisy. So was my own [personal website.](https://shafin.me).


# Quick Start 

1. Clone Git Repository

    > git clone https://github.com/shafinsiddique/daisy.git

2. Build sample site
    > dune exec -- daisy --root "./daisy-landing"

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

    (( usepartial (concat (.Site.root) "/layouts/partials/head.html") ))

- Reference the Markdown file's content

    (( .Page.content ))

- Create a local variable
    (( partialHeadUrl := (concat (.Site.root) "/layouts/partials/head.html")))

- Ternary Operator

    (( baseUrl := ?? .Site.prod -> .Site.baseUrl : "emptyUrl" ))


