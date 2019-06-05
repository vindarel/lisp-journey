+++
date = "2019-05-25T07:51:49+01:00"
title = "Python VS Common Lisp"
draft = false
+++

<!-- date = "2017-02-05T07:51:49+01:00" -->

¡ THIS IS A WORK IN PROGRESS !


I learned Java and C at school, I learned Python by myself and it was
a relief. After 5-6 years working and doing side projects in Python
and JS (mostly web dev, Django/Flask/AngularJS/Vuejs), I find the
experience so unsatisfactory that I'm switching to Common
Lisp.

I am not here to compare languages themselves, but their inherent
workflow and their ecosystem.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->

**Table of Contents**

- [Development process](#development-process)
    - [Interactivity](#interactivity)
    - [Editing code](#editing-code)
    - [Running, testing programs](#running-testing-programs)
    - [No types](#no-types)
    - [Refactoring](#refactoring)
- [Libraries](#libraries)
    - [Library management](#library-management)
    - [State of the libraries](#state-of-the-libraries)
- [Web](#web)
    - [Templates](#templates)
- [SQL composition](#sql-composition)
- [Deployment](#deployment)
    - [Shipping](#shipping)
    - [Performance](#performance)
- [Appendix A: FAQ](#appendix-a-faq)
    - [Is there no iterators ?](#is-there-no-iterators-)
    - [Can I define my own `+` operator like in an OO language ?](#can-i-define-my-own--operator-like-in-an-oo-language-)
    - [To which extent can Lisp be compiled, with all its dynamic nature, garbage collection, macros and what else ?](#to-which-extent-can-lisp-be-compiled-with-all-its-dynamic-nature-garbage-collection-macros-and-what-else-)
    - [But what is Common Lisp good for, really ?](#but-what-is-common-lisp-good-for-really-)
    - [So why is CL not more popular ?](#so-why-is-cl-not-more-popular-)

<!-- markdown-toc end -->


# Development process

## Interactivity

**Python**: restart everything at each code change, use breakpoints: this
 takes some time, is boring, requires to re-manipulate data to
 re-reach the state we were at to analyze and debug our program. We
 might figure out a non-standard, more interactive way, but still: a
 web server needs to restart, object instances don't get updated after
 a class definition.

**Common Lisp**: everything is so more interactive in the REPL. Even
developping web apps. On an error, we get an interactive debugger with
the stacktrace in our editor, we press `v` and voilà we are at the
problematic line. (for those who wonder, there are of course
production settings!) No need to restart any process. The variables
that we define on the REPL stay here. If we change a class definition
(say, we remove a field), existing instances get (lazily) updated.

## Editing code

Python: edit code line by line. Try out half-backed editor plugins to
edit code by semantic units.

Common Lisp: edit code by semantic units. I love emacs'
[lispy mode](http://oremacs.com/lispy/) (weird at first of course, but
so convenient).

(Actually, we edit code by parenthesis units, which doesn't carry as much meaning as an AST.)

I wrote a little plugin to help editing python code by manipulating
the AST ([red4e](https://github.com/vindarel/redbaron4emacs)).  We
first need an AST parser. There was one for py2, one for py3 without
annotations, eventually one emerged (unstable language and ecosystem,
more work). I went the simple way by calling each function into a new
python process, which is too
slow. [traad](https://github.com/abingham/traad) is a better project,
it can do much more but still, it's difficult to answer questions like
"who calls this function" (which is built-in in Slime). Maybe other
editors and proprietary ones come with a better experience, at the
cost of freedom, money, configuration time and CPU resources). Traad
went with a client-http server approach… that reminds me of Lisp's
Swank server !

## Running, testing programs

Python: run commands in the terminal. Scroll, read the output,
copy-paste manually (or use non-optimal termux or a terminal inside
emacs), go back to your editor. Type commands without completion (you
can't complete a test name), type the whole path to a singel unit test
(`pytest path/to/test.py::foo`).

Common Lisp: everything is interactive into your IDE (notably if you
like Emacs). Built-in completion for everything. We don't have to use
the shell (except from once in a while to run global tests or build
the system) and that's a good thing. Interactive
debugger. Interactively fix and re-run code and tests.


## No types

Python: you catch a lot of type errors in production, and/or you have
to write a lot more unit tests.

Now with type annotations: not stable, a different pipeline, not
interactive (unless specific IDE?).

In Common Lisp, particularly with SBCL, we get a lot of type errors or
warnings at compile time (`C-c C-c` on a *function*). That, plus
CLOS's generic dispatch (polymorphism), and Lisp's and CLOS' ability
to refactor, build a "DSL" to express what you want, makes a way more
comfortable experience. We're closer (not there, just closer) to the
"if it compiles, it works" situation.

Will [ML embedded into CL](https://github.com/tarballs-are-good/coalton)
help even more ?

## Refactoring

Python: you just can't refactor code as you want. Decorators, context
managers: they're limited to what they offer.

Common Lisp: just use macros, be concise and do what you want. We have the
decorator syntax (cl-annot) if we want.


# Libraries

## Library management

pip: use virtual envs (virtualenv, virtualenvwrapper, tox, anaconda,…
?), pin dependencies (pip-tools, pipenv, poetry, pyupdate,… ?). Debug
problems due to a third party library that didn't pin its dependencies
strictly enough.

quicklisp: think of it like Debian's apt, shipping releases that work
together (load together), and that we upgrade together. If needed, we
can still clone projects into `~/quicklisp/local-projects/` or have
project-local dependencies with Qlot. In my experience, so far so
good.

We are not even limited to Quicklisp any more ! The
[Ultralisp](http://ultralisp.org/) distribution builds every 5
minutes.


## State of the libraries

Suuuure, the Python ecosystem is huge.

CL's not so bad, see https://github.com/CodyReichert/awesome-cl and http://quickdocs.org/ :)

Above all else, Common Lisp is a stable language, and the libraries play this
game.

I believe Lisp's simpler syntax plays a good role on stability.

In his appreciated article [A Road to Common Lisp](http://stevelosh.com/blog/2018/08/a-road-to-common-lisp/), [Steve Losh](https://github.com/sjl) writes:

> as you learn Common Lisp and look for libraries, try to suppress the voice in the back of your head that says “This project was last updated six years ago? That’s probably abandoned and broken.” The stability of Common Lisp means that sometimes libraries can just be done, not abandoned, so don’t dismiss them out of hand.

[hacker news comments](https://news.ycombinator.com/item?id=17852194)


# Web

## Templates

Ever had headaches with Jinja ? Ever fought against Django
templates ? Ever abandoned to clean up a big mess of html and
templating code ? Used Jinja macros to factorize code ? Maybe you
turned to the good looking Jade (Pug). So you read another documentation,
you install tools to integrate it into your project. And now damn, no
more cross-files macros. You edit blocks of code whitespace by
whitespace. And in the end, your html may still not be valid…

In CL, we can use [Djula](https://github.com/mmontone/djula)
templates, a port of Django templates. FYI, despite its modest number
of stars, it is one of the most downloaded projects on Quicklisp. We
can alternatively just use plain old Lisp, for example with
[Spinneret](https://github.com/ruricolist/spinneret/). As a
consequence, factorize code as you always do (spinneret functions or
lisp macros). Manipulate code as you always do. It even warns on
malformed html and has some neat features (it is clever about headers
levels, it can embed markdown, etc).

# SQL composition

Python's ORMs (while solid, well established etc) all come with
limitations, idiosyncracies, and build their own DSL to build
non-trivial SQL queries. See the `Q` and `F` objects of the Django
ORM. Strange, isn't it ?

Why not use s-expressions and stay closer to SQL ? See
[clsql](http://clsql.kpe.io/manual/) and
[sxql](https://github.com/fukamachi/sxql) (from the
[Mito](https://github.com/fukamachi/mito) ORM).


# Deployment

## Shipping

Shipping an app, even more a web app, in Python (and JS) is extremely
tedious. We are far from shipping a self-contained executable. Current
projects aiming at fixing that are piles of hacks. Ok, I can install
it once and build on it. But several times ? And delevering an app to
end users ?

So we must turn to containers: ok, they're the Big Thing, but we still
need to spend hours on reading resources, building the Dockerfile, the
deployment pipeline, fixing upstream bugs, updating the stack,
accepting security holes, etc. Hours we could put on our app. And
still, *users can't download a binary*.

In Common Lisp: compile your program to machine code, embed the runtime, the
debugger, the web server, the JS dependencies, and ship it. Run it in
your server and access it from the outside straight away.

We can still use and benefit from Docker if needed, of course !

*Deployment process in Python*: install python and pip, install pip
dependencies and their system requirements (or try non-standard tools,
like Platter), install npm or yarn, install npm dependencies, build
the static files (or find a better, non-standard way to ship static
files), configure a server for static files (whitenoise, nginx), run a
WSGI webserver (gunicorn),…

*Deployment in CL*: build your binary, send it to the server, run it.

Shipping an Electron app in Python ? Not possible (or pile of
hacks). In CL: [Ceramic](https://github.com/ceramic).

## Performance

Python is notoriously slow, and passed the hobby project you quickly
realize that.

Python has a Global Interpreter Lock.

SBCL compiles to machine code.


As a consequence, you may not need memcached yet in your Lisp project.

For more insight, see the pgloader story below.


# Appendix A: FAQ

## Is there no iterators ?

In practice, we mostly rely on closures, but there are libraries to
create iterators.

See https://stackoverflow.com/questions/32956033/is-there-a-straightforward-lisp-equivalent-of-pythons-generators

## Can I define my own `+` operator like in an OO language ?

By default, no, because the CLOS came after the language specification
and thus everything isn't object-based. However there are libraries
like [generic-cl](https://github.com/alex-gutev/generic-cl/) and, in
practice, we quickly forget about this. Different operators is also a
means for performance, good type inference and error messages.

## To which extent can Lisp be compiled, with all its dynamic nature, garbage collection, macros and what else ?

Many Lisp compilers compile to machine code (SBCL, CCL, CMUCL,…).

Full answer: https://stackoverflow.com/questions/913671/are-there-lisp-native-code-compilers/914383#914383


## But what is Common Lisp good for, really ?

ok:

> Please don't assume Lisp is only useful for Animation and Graphics, AI, Bioinformatics, B2B and Ecommerce, Data Mining, EDA/Semiconductor applications, Expert Systems, Finance, Intelligent Agents, Knowledge Management, Mechanical CAD, Modeling and Simulation, Natural Language, Optimization, Research, Risk Analysis, Scheduling, Telecom, and Web Authoring just because these are the only things they happened to list. -- Kent Pitman

Kent Pitman

http://www.nhplace.com/kent/quoted.html

See also https://common-lisp.net/features.


## So why is CL not more popular ?

First, some reminders:

- success stories: http://lisp-lang.org/success/ Aircraft analysis suits, Missile defense, ICAD, music composition, algebra systems, bulk importer for PostgreSQL, grammar checking, 3D editor, knowledge graphs,…
- did you know that [pgloader](https://tapoueh.org/blog/2014/05/why-is-pgloader-so-much-faster/) was re-written from Python to Common Lisp ?
- CL was [used in a spacecraft](https://www.youtube.com/watch?v=_gZK0tW8EhQ&feature=youtu.be&t=4175) (and the repl was used to debug the system live from the earth)
- some companies still use and pick CL: https://github.com/azzamsa/awesome-lisp-companies
- reddit v1 was written in CL !
- CL was number 2 on the Tiobe index for years in the 80s !

That being said, my 2 cents since you ask:

- I think the CL world missed the web bandwagon for some time ([common-lisp.net](http://common-lisp.net/) was horrible for some years), but that's being fixed.
- we missed visually nice, practical content on the web (fixed or at least better)
- CL missed a package manager for some time behind other languages, that's now fixed.
- still quite hard for the web, hence no hype
- seems to be used for big, non-trivial projects, hence no easy hype
- no entity doing marketing (we are seeing the Common Lisp fundation pairing with sponsors now)
- other reasons: it may be hard (or harder than the concurrence) to grasp and getting started, lisp isn't for everyone, a lot of FUD, and a Lisp curse !

but that's all debattable, I wouldn't focus much on this.
