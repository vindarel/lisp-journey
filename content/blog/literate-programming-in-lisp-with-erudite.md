---
title: "Literate Programming in Lisp With Erudite"
date: 2020-03-21T11:01:57+01:00
draft: false
---

[Erudite](https://github.com/mmontone/erudite/) is a Common Lisp library to write literate programs.
The latest release (march, 2020) brings cool new features, amongst which the
ability to capture and print code output.

This page was created with Erudite. You can follow along with its
source [here](/blog/literate-erudite.lisp). Blogging about a programming language in the language itself is
pretty awesome and convenient (no more copy-pasting of code snippets
and manual adjustements of the indentation, yay!). It brings us closer
to an interactive notebook, even if it isn't the first goal.

# Basic usage


You write a lisp program, as usual. There is no extra step to produce the program sources,
since we are inside the sources. This is different than the Org-mode approach for example.

The comments will be the documentation. Comments inside a function are
also extracted and cut the function in two.

Erudite can export to Latex, RestructuredText, Markdown, HTML… and
actually to and from any format by using a "pass-through" directive.

<br>
Top level comments are shown like this. Here's code:

```lisp
(defun fibonacci (n &aux (f0 0) (f1 1))
  "docstring"
  (case n
    (0 f0)
    (1 f1)
```
this is an inline comment (there might be settings to control how it is rendered)

```lisp
    (t (loop for n from 2 to n
          for a = f0 then b and b = f1 then result
          for result = (+ a b)
          finally (return result)))))

```


Erudite defines directives to play with the output, such as `ignore` and `eval`. Note that directives start with a `@` sign, which I cannot use here.

With `ignore`, we can write lisp code but hide it from the output. And with `eval`, Erudite connects to a Swank server, captures and prints the output.

There's also the handy `code`, to write a snippet inside comments (so it is not part of
the Lisp source) and make it appear in the generated document.

For the text markup, we can use Erudite's
syntax ("link", "section", "subsection", "emph"…), or the markup of
the output file format.


# Evaluating code


With the latest Erudite, we can evaluate code. Note that it's a work
in progress.

The code snippet must be inside the comments too.

Here I call Fibonacci defined above: Fibonacci of 10 is…
<br>
55
<br>

You might need to create a Swank server first with


```lisp

(swank:create-server :dont-close t)
```

and tell Erudite its port if it isn't 4005.


```lisp

(setf erudite::*swank-port* 4005)
```

# Rendering the document


Call Erudite like so:

```lisp

(erudite:erudite #p"literal.md" "literal-erudite.lisp" :output-type :markdown)

```
We can also use a binary from the shell.

# Live rendering



I don't want to re-run this command everytime I want to see the generated document.
I use this snippet to automatically export my document when I save the Lisp source:

```lisp

(ql:quickload :cl-inotify)

(bt:make-thread
 (lambda ()
   (cl-inotify:with-inotify (inotify t ("literal-erudite.lisp" :close-write))
     (cl-inotify:do-events (event inotify :blocking-p t)
       (format t "~a~&" event)
       (erudite:erudite #p"literal.md" "literal-erudite.lisp" :output-type :markdown))))
 :name "inotify-erudite")

```

And then I make the markdown file to be live-rendered in the browser.
I used impatient-mode for Emacs (see [Wikemacs](http://wikemacs.org/wiki/Markdown#Live_preview_as_you_type)) with the help of `M-x auto-revert-mode`.

Kuddos to Mariano Montone!
