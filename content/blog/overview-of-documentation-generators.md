---
title: "Overview of Documentation Generators"
date: 2018-11-07T18:34:11+01:00
draft: false
---

I have a simple need: I'd like to generate an html documentation from
my code. What options do we have ?

I searched for "documentation tool" on Quickdocs:
http://quickdocs.org/search?q=documentation%20tool, from which I
remove old ones (clod, qbook, manifest).

I had two pure Lisp solutions working out of the box, two more are of
interest, and there's another non-Lisp of interest.

**update**: just found out that [qbook](http://quickdocs.org/qbook/)
([github mirror](https://github.com/lisp-mirror/qbook))is used for the
documentation of Fiveam, which is pretty nice:
https://common-lisp.net/project/fiveam/docs/index.html It can produce
html and latex. It uses docstrings and comments that start with 4
commas to structure the page
([exple in source](https://github.com/sionescu/fiveam/blob/master/src/suite.lisp)
that gives
[this](https://common-lisp.net/project/fiveam/docs/Test_0020Suites.html)). Running
it has a lot of asdf deprecation warnings and it did not work out of
the box for me ("The slot IT.BESE.QBOOK::GENERATOR is unbound in the object").


## Codex

Codex produces nice html but isn't automatic.

- https://github.com/CommonDoc/codex (by @eudoxia)
- example: https://commondoc.github.io/codex/docs/tutorial.html
- input fromat: [scriba](http://commondoc.github.io/scriba/docs/reference.html) by default. No more format it seems.
- output: multiple html files.
- rendering: modern, colored, light, TOC on the side
- granularity: good
- link to CLHS: yes
- totally automatic: no (one needs to create the documentation structure in `manual.lisp`, AFAIU).
- used in the wild: yes

Getting started: write a simple `docs/manifest.lisp` and `docs/manual.lisp`.

Am I mistaking ? It's exclusively manual. We must supply every
function/generic function/method/class/macro to document in
`manual.lisp`.


## Coo

Coo is a new tool in town, it works out of the box and it is actively developed !

- https://github.com/fisxoj/coo
- https://fisxoj.github.io/coo/
- input: docstrings in `rst`
- output: multiple html
- rendering: black & white, no TOC (active development)
- links to CLHS: yes, since yesterday :)
- granularity: doesn't show class slots, doesn't show generic functions.
- used in the wild: coo no, more probably cl-docutils.

Based on [cl-docutils](https://github.com/willijar/cl-docutils).

Displays the functions' documentation following their order in source (or not ? I saw exceptions).

It's straightforward:

    (coo:document-system :my-system)

and it produces html into `docs/`.


## Staple (doesn't work on Debian's SBCL 1.2.4)

You may be familiar with Staple since it's used by Shinmera in all his projects.

- https://github.com/Shinmera/staple
- output: html. The documentation string is plain text (no markup, rendered in a `<pre>` tag).
- cross-references
- can use a template.
- more features than listed here.

It doesn't support SBCL 1.2.4, so my tests fell short (upgrading isn't
100% smooth here). If you're on SBCL >= 1.4.8 Staple is a good option.

## Declt

Declt has higher goals than "quick and easy" documentation generator.

- https://github.com/didierverna/declt
- https://www.lrde.epita.fr/~didier/software/lisp/declt/
- output: a `.texi` file, that we can render into other formats (html, pdf).
- cross-references: yes

It didn't work out of the box (and had no explicit error information)
and it's also too involved for my use case.

## Documentation-tool (not for general use)

It's the template used for Edi Weitz software, like Hunchentoot.

- https://edicl.github.io/documentation-template/
- output: one html file

It works, but it thinks you publish an edicl software and has some hardcoded "http://weitz.de/files/…" urls.


## Tinaa (unmaintained)

I liked the output, but it didn't work (asdf-related error), and it's unmaintained (authors' words).

- https://github.com/gwkkwg/tinaa
- https://common-lisp.net/project/tinaa/documentation/index.html
- output: html


## See also

### cl-domain (Sphinx)

Another excellent option is 40ants' cldomain, which builds on Python's proven Sphinx:

> CLDomain is an extension for the Sphinx documentation generation tool that allow sphinx to generate documentation for Common Lisp libraries. Documentation is extracted from the various entity’s documentation strings, loaded from ASDF systems and associated internal packages.

They use it for they new projects since around 3 years now.

- https://github.com/40ants/cldomain
- http://40ants.com/cldomain/
- input: rst
- output: many html
- HyperSpec links: yes
- requirements: Python, pip


*udate* 2019-01-22: extended example.

An example from [cl-hamcrest](https://github.com/40ants/cl-hamcrest/blob/master/docs/source/matchers.rst):

- we reference Lisp functions/methods/macros with RST directives:

```
Object matchers
===============

This kind of matchers checks some sort of properties on an object, etc.

.. cl:package:: hamcrest/matchers

.. cl:macro:: has-plist-entries
```

- they have RST docstrings, also with RST directives to include code blocks:

~~~lisp
(def-has-macro
    has-plist-entries
    "Matches plist entries:

.. code-block:: common-lisp-repl

   TEST> (let ((obj '(:foo :bar)))
           (assert-that obj
                        (has-plist-entries :foo \"bar\"
                                           :blah \"minor\")))
     × Key :FOO has :BAR value, but \"bar\" was expected

This way you can test any number of plist's entries."

    :check-obj-type (check-if-list object)
    :get-key-value (let ((key-value (getf object key 'absent)))
                      (when (eql key-value 'absent)
                          …
~~~

- this produces a nice output ([here](http://40ants.com/cl-hamcrest/matchers.html)):

![](/cl-domain.png)


---

I'll use and watch Coo !
