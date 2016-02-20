#BlaTeX

Markdown and HTML are the standard tools used to write your every day tech blog with. But they have pretty weak support for embedding mathematical formulas, and are not conducive to writing for an extended period of time. Plus, they aren't even Turing complete! So use BlaTeX to start blogging in LaTeX!

#Jekyll for LaTeX
BlaTeX is basically Jekyll for LaTeX and thus a lot of the same benefits apply:

 >~~Jekyll~~ BlaTeX is a simple, blog-aware, static site generator. It takes a template directory containing ~~raw text files in various formats~~ LaTeX files, ~~runs it through a converter (like Markdown) and our Liquid renderer,~~ and spits out a complete, ready-to-publish static website suitable for serving with your favorite web server. ~~Jekyll also happens to be the engine behind GitHub Pages, which means~~ you can use ~~Jekyll~~ BlaTeX to host your project’s page, blog, or website from GitHub’s servers for free.

Also, because I was such a big fan of [The Hyde Theme](https://github.com/poole/hyde), I ended up porting it for myself one night. If you have a Jekyll theme you're really attached to, it shouldn't be too difficult to do the same. 

#Requirements

 - [The Haskell Platform](https://www.haskell.org/platform/)
 - [LaTeX](https://latex-project.org/ftp.html)

#How To

First install [BlaTeX](http://hackage.haskell.org/package/blatex-0.1.0.5) by running `cabal install blatex`. Then create the directory you want your blog to be based in, initialize a git repository, etc.

When you're ready, from that directory run `blatex init`. That will create a skeleton file structure like this:

```bash
.
├── index.html.bltx
└── posts	
    ├── example-post.pdf
    └── example-post.tex	
```

Now finally you can do `blatex build` to generate your site. If that succeeds, you will have the example blog up and running!

#Customizing

The `index.html.bltx` will basically be the layout you want for the homepage of your blog. Just make sure it is a valid HTML file (with Javascript, CSS, whatever you want) and make sure to keep the following HTML element wherever you want the list of your blog posts to go:

```html
<ul id="blog-posts"></ul>
```

Each blog post will be an `li` element (with the class `blog-post`) containing an `a` tag to the post. The `a` tag will have the title of that blog post (which is specified in the LaTeX file as usual with `\title{WHATEVER THE TITLE IS}`).

If you want an example of a decent looking blog, rather than just the skeleton, you can use [this example `index.html.bltx` file](https://github.com/2016rshah/thoughts/blob/gh-pages/index.html.bltx). 

Obviously, your own blog would need its own posts too! I don't know how to help you with content, you'll need to figure that out yourself, but when you do you will be able to write your posts in the posts directory as LaTeX files. You will also need to compile them yourself (with `pdflatex` probably) just like you normally would and make sure the resulting PDFs look nice. You need to ensure that you include a `\title` and a `\date`. Make sure you format the date as `\date{1 January 2016}` otherwise BlaTeX won't find your post! When you're satisfied, you can run `blatex build` again to update the blog. 

If you don't want to run `blatex build` every time you change a file, look into [SOS](https://github.com/schell/steeloverseer) or [Gulp](http://gulpjs.com/). 

#Todo
 - ~~Templates for index page that I can insert HTML into~~
 - Modularize code to separate different tasks (extracting info from LaTeX files, creating HTML elements, inserting HTML into layout, etc.)
 - ~~Insert the generated HTML into the layout file~~
 - ~~Push to hackage~~
 - ~~Add command line options~~
 - ~~`blatex init` - Add command line option to set up a bare-bones blog with dummy text and stuff~~
   - ~~Create a basic template HTML file that's decently pretty, but basic~~
   - ~~Create a couple sample blog posts~~
   - ~~Be able to create them from Haskell~~
 - ~~Format the code before inserting it into the HTML~~
 - `blatex watch` - Constantly watch layout and posts directory to rebuild on change
 - ~~Sort posts by the date provided in LaTeX file~~