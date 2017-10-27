---
title: "Installing libraries, dependencies management"
date: 2017-10-27T11:01:31+02:00
draft: false
---

Common Lisp may have more libraries than you think. See:

* [Quickdocs](http://quickdocs.org/) - the library documentation hosting for CL.
* the [Awesome-cl](https://github.com/CodyReichert/awesome-cl) list, a
  curated list of libraries.
* [lisp-lang.org's recommended libraries](http://lisp-lang.org/wiki/article/recommended-libraries) (from [State of the CL ecosystem, 2015](http://borretti.me/article/common-lisp-sotu-2015))

Quicklisp is the de-facto package manager, but not the only tool.


## Some terminology first

* In the Common Lisp world, a **package** is a way of grouping symbols
together and of providing encapsulation. It is similar to a C++
namespace, a Python module or a Java package.

* A **system** is a collection of CL source files bundled with an .asd
  file which tells how to compile and load them. There is often a
  one-to-one relationship between systems and packages, but this is in
  no way mandatory. A system may declare a dependency on other
  systems. Systems are managed by [ASDF](https://common-lisp.net/project/asdf/asdf.html) (Another System Definition
  Facility), which offers functionalities similar to those of make and
  ld.so, and has become a de facto standard.

* A Common Lisp library or project typically consists of one or
  several ASDF systems (and is distributed as one Quicklisp project).

## Install Quicklisp

[Quicklisp](https://www.quicklisp.org/beta/) is more than a package
manager, it is also a central repository (a *dist*) that ensures that
all libraries build together. This involves some manual work (like
reporting errors to package authors), so this is why Quicklisp
releases its dist updates once a month (but fear not, we have other
tools).

It provides its own *dist* but it is also possible to build our own.

To install it, we can either:

1- run this command, anywhere:

    curl -O https://beta.quicklisp.org/quicklisp.lisp

and enter a Lisp REPL and load this file:

    sbcl --load quicklisp.lisp

or

2- install the Debian package:

    apt-get install cl-quicklisp

and load it, from a REPL:

~~~lisp
(load "/usr/share/cl-quicklisp/quicklisp.lisp")
~~~

Then, in both cases, still from the REPL:

~~~lisp
(quicklisp-quickstart:install)
~~~

This will create the `~/quicklisp/` directory, where Quicklisp will
maintain its state and downloaded projects.

If you want Quicklisp to always be loaded in your Lisp sessions, run
`(ql:add-to-init-file)`: this adds the right stuff to the init file of
your CL implementation. Otherwise, you have to run `(load
"~/quicklisp/setup.lisp")` in every session if you want to use
Quicklisp or any of the libraries installed through it.

It adds the following in your (for example) `~/.sbclrc`:

~~~lisp
#-quicklisp
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init)))
~~~

## Install libraries

In the REPL:

~~~lisp
(ql:quickload "package-name")
~~~

and voilà. See Quicklisp's documentation for more commands.


Note also that dozens of Common Lisp libraries are packaged in
Debian. The package names usually begin with the cl- prefix (use
`apt-cache search --names-only "^cl-.*"` to list them all).

For example, in order to use the CL-PPCRE library (for regular
expressions), one should first install the `cl-ppcre` package.

Then, in SBCL and ECL, it can be used with:

~~~lisp
(require "asdf")
(require "cl-ppcre")
(cl-ppcre:regex-replace "fo+" "foo bar" "frob")
~~~

See more: https://wiki.debian.org/CommonLisp

## Advanced dependencies management

Quicklisp installs the libraries into `~/quicklisp/local-projects/`. A
library installed here is automatically available for every project.

### Providing our own version of a library. Cloning projects.

Given the property above, we can clone any library into the
local-projects directory and it will be found by quicklisp and
available right-away:

~~~lisp
(ql:quickload "package")
~~~

And also given the `M-.` "go to this symbol definition" feature in
Slime (and `M-,` to go back), it's really easy to not only explore but
start tweaking and extending other libraries.

### How to work with local versions of libraries

If we need libraries to be installed locally, for only one project, or
in order to easily ship a list of dependencies with an application, we
can use [Qlot](https://github.com/fukamachi/qlot). This is like
Python's virtual environments.

Quicklisp also provides
[Quicklisp bundles](https://www.quicklisp.org/beta/bundles.html). They
are self-contained sets of systems that are exported from Quicklisp
and loadable without involving Quicklisp.

At last, there's
[Quicklisp controller](https://github.com/quicklisp/quicklisp-controller)
to help us build *dists*. Some projects use this, like CL21.

## Read more

* Source code organization, libraries and packages:  [https://lispmethods.com/libraries.html](https://lispmethods.com/libraries.html)
* [https://wiki.debian.org/CommonLisp](https://wiki.debian.org/CommonLisp)

## See also

* [Qi](https://github.com/CodyReichert/qi) - a package manager for Common Lisp
