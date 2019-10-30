+++
date = "2019-10-30T07:51:49+01:00"
title = "Python VS Common Lisp, workflow and ecosystem"
draft = true
+++

<!-- started = "2017-02-05T07:51:49+01:00" -->
<!-- finished = "2019-10-30" -->


I learned Java and C at school, I learned Python by myself and it was
a relief. After 8 years working and doing side projects in Python
and JavaScript (mostly web dev, Django/Flask/AngularJS/Vuejs), I find the
experience so unsatisfactory that I'm switching to Common
Lisp.

I am not here to compare languages themselves, but their inherent
workflow and their ecosystem. Python might not be as nice as you
think, and Common Lisp not as bad as you were convinced after a quick look.

Let's dive in.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
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
- [Templates](#templates)
- [SQL composition](#sql-composition)
- [Deployment, Shipping](#deployment-shipping)
- [Performance](#performance)
- [Conclusion](#conclusion)
- [Appendix A: FAQ](#appendix-a-faq)
    - [Are there no iterators ?](#are-there-no-iterators-)
    - [Can I define my own `+` operator like in an OO language ?](#can-i-define-my-own--operator-like-in-an-oo-language-)
    - [To which extent can Lisp be compiled, with all its dynamic nature, garbage collection, macros and what else ?](#to-which-extent-can-lisp-be-compiled-with-all-its-dynamic-nature-garbage-collection-macros-and-what-else-)
    - [But what is Common Lisp good for, really ?](#but-what-is-common-lisp-good-for-really-)
    - [So why is CL not more popular ?](#so-why-is-cl-not-more-popular-)

<!-- markdown-toc end -->


# Development process

## Interactivity

**Python**: we typically restart everything at each code change, we use breakpoints: this
 takes some time, is boring, it requires to re-manipulate data to
 re-reach the state we were at to analyze and debug our program. We
 might figure out a non-standard, more interactive way, but still: a
 web server needs to restart, object instances don't get updated after
 a class definition.

**Common Lisp**: everything is so more interactive in the REPL. Even
developping web apps. On an error, we get an interactive debugger with
the stacktrace in our editor, we press `v` and voilà, we are at the
problematic line. We can of course catch errors to avoid the debugger, or disable it with global settings. We don't need to restart any process. The variables
that we define on the REPL stay here. If we change a class definition
(say, we remove a field), existing instances get (lazily) updated.

The Lisp REPL is part of the development process, it is not only used
for exploration and debugging. It's fun, it's a productive boost, and
it allows to catch errors earlier, both because we try functions
earlier, and because we get type warnings when we compile the file or
the current function.

Here's a video where the developer defines a dummy interface, makes it
fail, develops it, and tests it, all quickly by interacting with the REPL.

<iframe width="560" height="315" src="https://www.youtube.com/embed/CNFr7zIfyeM" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

## Editing code

*Python*: we edit code line by line, paragraph by paragraph. We can
try out half-backed editor plugins to edit code by semantic units. We
must even pay attention to add a couple whitespace there, remove one
there. We are from the immediate interactive feedbak of the hacker's
vision "Inventing on Principles".

*Common Lisp*: we edit code by semantic units. I love emacs' [lispy
mode](http://oremacs.com/lispy/), which is weird at first of course,
but so convenient. We can navigate to expressions back and forth, we
can delete a whole "if" statement with a keypress, indentation is
automatic, etc.

Actually, we edit code by parenthesis units, which doesn't carry as
much meaning as an AST. For a real AST, we'd need a code walker. But
since Lisp's syntax is based on parenthesis, in practice the
experience is similar.

I wrote a little plugin to help editing python code by manipulating
the AST ([red4e](https://github.com/vindarel/redbaron4emacs)).  We
first need an AST parser. There was one for Python 2, another one for
Python 3 without annotations, eventually one emerged a couple years
later, signs of an unstable language and ecosystem, more work required
by the developer. I went the simple way by calling each function into
a new python process, which is of course too
slow. [traad](https://github.com/abingham/traad) is a better project,
it can do much more but still, it's difficult to answer questions like
cross-referencing: "who calls this function" or "who does
this function call", which are built-in in Slime.

Maybe other editors and proprietary ones come with a better
experience, at the cost of freedom, money, configuration time and
memory and CPU resources. If I have the choice, I'd prefer to not go
this route, and choose a better platform from the start.

Traad is built around a client-http server approach…  that reminds me
of Lisp's Swank server, the backend of Slime! A sign that Swank/Slime
has the right architecture for modern times since its inception. It is
based on a stable language whose syntax can not rot and has decades of
development behind it.

Slime itself is tied to Emacs, and thus a newcomer can find the UI
unpractical. Swank though can be used outside of Emacs, and it is for
example for Atom's [Slima](https://github.com/neil-lindquist/SLIMA/),
which now has all the most important Slime features: REPL, integrated
debugger, jump to definition, autocompletion, interactive object
inspection, and more.

- more: https://lispcookbook.github.io/cl-cookbook/editor-support.html (Eclipse, Lem, Jupyter Notebook,…)


## Running, testing programs

*Python*: the default workflow is to run commands in the
terminal. Scroll, read the output, copy-paste manually (or use the
non-UX-optimal termux or a terminal inside emacs), go back to your
editor. Type commands without completion, type the whole path to a
single unit test (`pytest path/to/test.py::foo`), or configure your
editor (and don't miss good plugins, like I realized while asking for
feedback on the article).

*Common Lisp*: the default workflow is to do everything interactively
into the REPL (notably if you like Emacs), but some people still use a
write-compile-run approach. We have built-in completion for
everything. We don't have to use the shell (except from once in a
while to run global tests or build the system) and that's a good
thing. Interactive debugger. Interactively fix and re-run code and
tests.

Here's a quick demo on how to interactively fix failing tests ([direct link](https://peertube.video/videos/watch/c0c82209-feaa-444d-962f-afa25745bfc0)):

<iframe width="560" height="315" sandbox="allow-same-origin allow-scripts" src="https://peertube.video/videos/embed/c0c82209-feaa-444d-962f-afa25745bfc0" frameborder="0" allowfullscreen></iframe>

Running and debugging on a remote server: in Python, we usually simply
rsync sources and run tests manually, or start vim/emacs under tmux on
the server. We have to kill the app to reload it. In Common Lisp, we
can connect to the running, remote instance, write changes locally,
hit `C-c C-c` on a function and see changes on the remote image. CL
has more hackerish capacities here, no doubt!

- more information on (remote) debugging: https://lispcookbook.github.io/cl-cookbook/debugging.html


## No types

*Python*: we catch a lot of type errors in production, and/or we have
to write a lot more unit tests. It's a fact, do we agree?

Now we can improve the situation somehow with type annotations,
however: the feature is not stable (do you want mypy, the new typing
module, pyre?), it's a different pipeline, it is not interactive by
default, we need to configure our IDE.

In *Common Lisp*, particularly with SBCL, we get a lot of type errors
or warnings at compile time (`C-c C-c` on a *function*).
We're closer (not there, just closer) to the "if it
compiles, it works" situation.

- https://lispcookbook.github.io/cl-cookbook/type.html
- [Compile-time type checking in the programmable programming language Lisp](https://medium.com/@MartinCracauer/static-type-checking-in-the-programmable-programming-language-lisp-79bb79eb068a)
- will [ML embedded into CL](https://github.com/tarballs-are-good/coalton)
help even more ?

## Refactoring

*Python*: we just can't refactor code as we want. Decorators, context
managers: they have an interface and they are limited to what they offer.

*Common Lisp*: we can use macros, be concise and do what we
want. We can have the decorator syntax (and any other) if we want
(with the cl-annot library, or by writing our reader macros). It's not
only macros though. The polymorphism of the object system (CLOS)
(commonly referred to as generic dispatch) helps, and Lisp's
"moldability" in a whole allows us to refactor code exactly how we
want, to build a "DSL" to express what we want.

# Libraries

## Library management

*pip*: use virtual envs (virtualenv, virtualenvwrapper, tox, anaconda,…
?), pin dependencies (pip-tools, pipenv, poetry, pyupdate,… ?). Debug
problems due to a third party library that didn't pin its dependencies
strictly enough.

*quicklisp*: think of it like Debian's apt, shipping releases that work
together (load together), and that we upgrade together. If needed, we
can still clone projects into `~/quicklisp/local-projects/` or have
project-local dependencies with [Qlot](https://github.com/fukamachi/qlot). In my experience, so far so
good.

We are not even limited to Quicklisp any more (it can be limiting
because it release cycle is of one month). The
[Ultralisp](http://ultralisp.org/) distribution builds every 5
minutes. [clpm](https://gitlab.common-lisp.net/clpm/clpm) is a package
manager with a traditionnal approach.


## State of the libraries

CL might have more libraries than you think, see https://github.com/CodyReichert/awesome-cl and http://quickdocs.org/ or just search on Github :)

But sure, the Python ecosystem is huge. A few remarks:

- Quicklisp has around 1500 packages, PyPI over than 170 000. What are they
  all for? :D Even in CL have we dozens of test frameworks. It's hard to believe PyPI has a hundred times more `useful` libraries.
- Quicklisp is a curated distribution, PyPI is not. That means that
  libraries that don't compile anymore are rejected (after a notice to
  the maintainers), and that orphan projects' url can be updated to
  point to a community maintained one.
- Anybody can easily publish a library to PyPI on its own. Less so
  with Quicklisp, one must open an issue.
- [numcl](https://github.com/numcl/numcl) is a Numpy clone.
- if needed, you can use [py4cl and more](https://github.com/CodyReichert/awesome-cl#python) to interface to Python.

An important remark, is that Common Lisp is a stable language, and
that the libraries play this game (I saw a deprecation feature staying
for 12 years).

I believe Lisp's simpler, non-roting syntax plays a good role on
stability. Caution: that doesn't mean the implementations don't
evolve, quite the contrary.

In his appreciated article [A Road to Common Lisp](http://stevelosh.com/blog/2018/08/a-road-to-common-lisp/), [Steve Losh](https://github.com/sjl) writes:

> as you learn Common Lisp and look for libraries, try to suppress the voice in the back of your head that says “This project was last updated six years ago? That’s probably abandoned and broken.” The stability of Common Lisp means that sometimes libraries can just be done, not abandoned, so don’t dismiss them out of hand.

[hacker news comments](https://news.ycombinator.com/item?id=17852194)


# Templates

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
consequence, factorize code as you always do (with spinneret functions or
lisp macros). Manipulate code as you always do. It even warns on
malformed html and has some neat features (it is clever about headers
levels, it can embed markdown, etc).


# SQL composition

Python's ORMs (while solid, well established etc) all come with
limitations, idiosyncracies, and build their own DSL to build
non-trivial SQL queries. See the `Q` and `F` objects of the Django
ORM. Strange, isn't it ?

Why not use s-expressions and stay closer to SQL ? See for example
[sxql](https://github.com/fukamachi/sxql) from the
[Mito](https://github.com/fukamachi/mito) ORM.

A Mito query looks like this:

```lisp
(select-dao 'tweet
    (where (:like :status "%Japan%")))
```

# Deployment, Shipping

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

In *Common Lisp*: compile your program to machine code, embed the runtime, the
debugger, the web server, the JS dependencies, and ship it. Run it in
your server and access it from the outside straight away.

An SBCL image of a non-trivial web project will weight ± 20 to 30MB
(with core compression). For a lighter binary (not that I care
personnally), we could try ECL (that compiles to C), or use
tree-shakers of proprietary implementations (LispWorks, Allegro).

We can still use and benefit from Docker if needed, of course !

*Deployment process in Python*: install python and pip, install pip
dependencies and their system requirements and be prepared for errors (or try non-standard tools,
like Platter), install npm or yarn, install npm dependencies, build
the static files (or find a better, non-standard way to ship static
files), configure a server for static files (whitenoise, nginx), run a
WSGI webserver (gunicorn),…

*Deployment in CL*: build your binary, send it to the server, run it.

# Performance

Python is notoriously slow, and passed the hobby project you quickly
realize that.

Python has a Global Interpreter Lock.

SBCL compiles to machine code.

We can fine-tune the types in our Lisp programs for the compiler to
make the consequent optimizations. We can run in "debugging first" or
in "speed first" modes. We can inline code to gain in the cost of
function calls.

As a consequence, you may not need memcached yet in your Lisp project.

- https://lispcookbook.github.io/cl-cookbook/performance.html
- [CL can be tuned to be faster than C](https://www.reddit.com/r/lisp/comments/1udu69/how_to_make_lisp_go_faster_than_cpdf/)
- interesting stuff:
  [Petalisp](https://github.com/marcoheisig/Petalisp) - an attempt to
  generate high performance code for parallel computers by
  JIT-compiling array definitions. It works on a more fundamental
  level than NumPy, by providing even more powerful N-dimensional
  arrays, but just a few building blocks for working on them.
- see the pgloader story below.

# Conclusion

I hope I killed some FUD and renewed your interest for Common Lisp. There are a lot happening right now, come join in!

You can reach the community here:

* [/r/Common_Lisp](https://www.reddit.com/r/Common_Lisp/) - subreddit about Common Lisp
* [common-lisp.net](https://common-lisp.net)
* [lisp-lang.org](https://lisp-lang.org/)
* [Lisp Discord Server](https://discord.gg/hhk46CE)
* [#lisp](http://log.irc.tymoon.eu/freenode/lisp) on Freenode - main Common Lisp IRC channel.
* [chat.lisp.cl](https://chat.lisp.cl/) - a Mattermost chat server, with bridges to IRC and Discord.
* [Planet List](http://planet.lisp.org/) - A meta blog that collects the contents of various Lisp-related blogs.


# Appendix A: FAQ

Some info every Python programmer will come accross eventually. Saves you some googling.

## Are there no iterators ?

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
- reddit v1 was written in CL ! JavaScript was written in CL !
- CL was number 2 on the Tiobe index for years in the 80s !

That being said, my 2 cents since you ask:

- I think the CL world missed the web bandwagon for some time ([common-lisp.net](http://common-lisp.net/) was horrible for some years), but that's being fixed.
- we missed visually nice, practical content on the web, even though
  there are many books (fixed or at least better)
- CL missed a package manager for some time behind other languages, that's now fixed.
- I reckon CL is still quite hard for the web, it doesn't have a killer web framework (though maybe Weblocks soon©), hence no hype.
- CL seems to be used for big, non-trivial projects, hence it gets no easy hype.
- we have no entity doing marketing. We are seeing the Common Lisp fundation pairing with sponsors now.
- other reasons: it may be hard (or harder than the concurrence) to
  grasp and getting started, lisp isn't for everyone, it gets a lot of
  FUD, and a so-called Lisp curse!

but that's all debattable, I wouldn't focus much on this. [Times are
good for
implementations](https://lisp-journey.gitlab.io/blog/these-years-in-common-lisp-2018/#implementations), that's what counts.
