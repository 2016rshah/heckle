![Heckle](meta/l3.png)

An easy to use static-site compiler written in Haskell that supports LaTeX/PDF **and** Markdown/HTML posts. Care has been taken to make it as simple and unopinionated as possible. 

In other words Heckle is basically **Jekyll in Haskell (feat. LaTeX)**.

#Why

Want to use Markdown? Cool. Want to use LaTeX? Cool. Want to use both? Cool. 

Also, Jekyll was [too OP](https://jekyllrb.com/docs/structure/) for my taste. I just wanted to [throw some posts in a directory](https://github.com/2016rshah/heckle#how-to) and be ready to roll. [My own blog](http://www.rshah.org/blog/) uses Heckle with a ported version of [The Hyde Theme](https://github.com/poole/hyde) so it looks like a Jekyll blog, it's more simple to maintain, and it supports LaTeX posts. 

#Requirements
 - [The Haskell Platform](https://www.haskell.org/platform/)
 - [LaTeX](https://latex-project.org/ftp.html)

(Currently [considering ways](https://github.com/2016rshah/heckle/issues/9) to make LaTeX an optional dependency)

#How To

First install [Heckle](https://hackage.haskell.org/package/heckle-2.0.0.0) by running `cabal install heckle`. Then create the directory you want your blog to be based in, initialize a git repository, etc.

When you're ready, from that directory run `heckle init`. That will create a skeleton file structure. 

Now finally you can do `heckle build` to generate your site. If that succeeds, you will have the example blog up and running!

It'll look a little like this:

```bash
.
├── index.html
├── index.html.hkl
├── template.html.hkl
└── posts
    ├── example-latex.pdf
    ├── example-latex.tex
    ├── example-markdown.html
    └── example-markdown.md
```

#Customizing

At this point, if you open the resulting `index.html` file, everything will look pretty awful. But luckily, you can customize just about everything. 

(Also, if you want to get up and running quickly with [the Hyde theme](https://github.com/poole/hyde), follow the instructions in [this README](https://github.com/2016rshah/blog). It will look like [my blog](http://www.rshah.org/blog/) rather than just the skeleton and you can edit things from there.) 

###Homepage

The `index.html.hkl` will basically be the layout you want for the homepage of your blog. Just make sure it is a valid HTML file (with Javascript, CSS, whatever you want) and make sure to keep the following HTML element wherever you want the list of your blog posts to go:

```html
<ul id="blog-posts"></ul>
```

Each blog post will be an `li` element (with the class `blog-post`) containing an `a` tag to the post and a `div` with the date. Heckle will generate something like this:

```html
<li class="blog-post">
    <a class="post-link" href="posts/example-post.pdf">
        Example Post
    </a>
    <div class="post-date">
        1 January 2015
    </div>
</li>
```

###LaTeX Posts

Heckle will find all the `.tex` and `.pdf` file pairs in the `posts/` directory and aggregate links to them in your homepage. You will need to compile them yourself (with `pdflatex` probably) just like you normally would and make sure the resulting PDFs look nice. 

**You need to ensure that you include a `\title` and a `\date` in the preamble of every LaTeX file**. Make sure you format the date as `\date{1 January 2016}` otherwise Heckle won't find your post! 

```tex
\date{1 January 2015}
\title{Example LaTeX Post}
```

When you're satisfied, you can run `heckle build` again to update the blog. 

###Markdown Posts

The `template.html.hkl` file is basically the layout file for your Markdown blog posts. Just make sure it is a valid HTML file (with Javascript, CSS, whatever you want) and make sure to keep the following HTML element wherever you want your blog post content:

```html
<div id="blog-post"></div>
```

**You need to ensure that you include commented meta-data (outlined below) including the title, author, and date in every Markdown file**. Make sure you format the date as `1 January 2016` otherwise Heckle won't find your post! The first three lines of each markdown file need to be formatted as follows (the date format is `1 January 2016`):

```markdown
% <TITLE>
% <AUTHOR>
% <FORMATTED-DATE>
```

Heckle will find all the `.md` files and convert them to `.html` with the help of [Pandoc](http://pandoc.org/). Then it will insert the resulting HTML into your `template.html.hkl` file at the specified location. Finally, it aggregates links to all the posts in your homepage. 

When you're satisfied, you can run `heckle build` again to update the blog. 

###Notes

If you don't want to run `heckle build` every time you change a file, look into [SOS](https://github.com/schell/steeloverseer) or [Gulp](http://gulpjs.com/). 

#BlaTeX ?
This project used to be called BlaTeX and was specifically for LaTeX/PDF posts (it did not support Markdown/HTML). It was created over the course of my senior year in high-school as my [senior research project](https://github.com/2016rshah/Meta-BlaTeX). Eventually I decided to [support Markdown as well](https://github.com/2016rshah/heckle/issues/5) and thus Heckle was born.

#See also:
  - The Hackage package: https://hackage.haskell.org/package/heckle
  - PDFs are the future ?¿?: http://www.rshah.org/blog/posts/what-is-the-web.pdf
  - The Abstraction Progression of BlaTeX: http://www.rshah.org/blog/posts/abstraction-progression-blatex.pdf
  - The culmination of BlaTeX in my final paper: https://github.com/2016rshah/Meta-BlaTeX/blob/master/Paper/final.pdf
  - My powerpoint presentations about Haskell/BlaTeX: https://github.com/2016rshah/Meta-BlaTeX/tree/master/Presentations
  - My poster for BlaTeX: https://github.com/2016rshah/Meta-BlaTeX/blob/master/Poster/Poster.pdf
  - The historical repo for BlaTeX: https://github.com/2016rshah/Meta-BlaTeX/tree/master/BlaTeX
  - How BlaTeX became Heckle: http://www.rshah.org/blog/posts/birth-of-heckle.html