#BlaTeX

Markdown and HTML are the standard tools used to write your every day tech blog with. But they have pretty weak support for embedding mathematical formulas, and are not conducive to writing for an extended period of time. Plus, they aren't even Turing complete! So use BlaTeX to start blogging in LaTeX!

#Jekyll for LaTeX
BlaTeX is basically Jekyll for LaTeX and thus a lot of the same benefits apply:

 >~~Jekyll~~ BlaTeX is a simple, blog-aware, static site generator. It takes a template directory containing ~~raw text files in various formats~~ LaTeX files, ~~runs it through a converter (like Markdown) and our Liquid renderer,~~ and spits out a complete, ready-to-publish static website suitable for serving with your favorite web server. ~~Jekyll also happens to be the engine behind GitHub Pages, which means~~ you can use ~~Jekyll~~ BlaTeX to host your project’s page, blog, or website from GitHub’s servers for free.

#How To
The tool is still under heavy development, but basically the idea is that starting a new blog will work like this:

```bash
$ mkdir posts; cd posts
$ touch post1.tex
$ emacs post1.tex
$ pdflatex post1
$ cd ..
$ blatex build
$ git add .; git commit -m 'Started a blog!'; git push origin gh-pages
```
This will create a blog with the default template that can be served by github pages.

#Todo
 - Templates for index page that I can insert HTML into
 - Modularize code to separate different tasks (extracting info from LaTeX files, creating HTML elements, inserting HTML into layout, etc.)
 - Insert the generated HTML into the layout file
 