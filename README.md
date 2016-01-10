#BlaTeX

Markdown and HTML are the standard tools used to write your every day tech blog with. But they have pretty weak support for embedding mathematical formulas, and are not conducive to writing for an extended period of time. Plus, they aren't even Turing complete! So use BlaTeX to start blogging in LaTeX!

#Jekyll for LaTeX
BlaTeX is basically Jekyll for LaTeX and thus a lot of the same benefits apply:

 >~~Jekyll~~ BlaTeX is a simple, blog-aware, static site generator. It takes a template directory containing ~~raw text files in various formats~~ LaTeX files, ~~runs it through a converter (like Markdown) and our Liquid renderer,~~ and spits out a complete, ready-to-publish static website suitable for serving with your favorite web server. ~~Jekyll also happens to be the engine behind GitHub Pages, which means~~ you can use ~~Jekyll~~ BlaTeX to host your project’s page, blog, or website from GitHub’s servers for free.

#How To

Create a skeleton file structure like this, and you can just run blatex in the root directory and push to gh-pages for a blog.

```bash
.
├── index.html.bltx
└── posts	
    ├── post1.pdf
    ├── post1.tex
    ├── post2.pdf
    └── post2.tex	
```

The `index.html.bltx` will basically be the layout you want for the homepage of your blog. Just make sure it is a valid HTML file (with Javascript, CSS, whatever you want) and include the following HTML element wherever you want the list of your blog posts to go:

```html
<ul id="blog-posts"></ul>
```

Each blog post will be an `li` element (with the class `blog-post`) containing an `a` tag to the post. The `a` tag will have the title of that blog post (which is specified in the LaTeX file as usual with `\title{WHATEVER THE TITLE IS}`).

After you have your layout file (in `index.html.bltx`) and your posts (in the `posts` directory), you can just do

```bash
$ cabal install blatex
$ blatex build
$ git add .; git commit -m 'Started a blog!'; git push origin gh-pages
```

#Todo
 - ~~Templates for index page that I can insert HTML into~~
 - Modularize code to separate different tasks (extracting info from LaTeX files, creating HTML elements, inserting HTML into layout, etc.)
 - ~~Insert the generated HTML into the layout file~~
 - ~~Push to hackage~~
 - ~~Add command line options~~
 - Add command line option called `init` to set up a bare-bones blog with dummy text and stuff
   - Create a basic template HTML file that's decently pretty, but basic
   - ~~Create a couple sample blog posts~~
   - Be able to create them from Haskell
 - Format the code before inserting it into the HTML