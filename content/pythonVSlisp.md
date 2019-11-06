+++
date = "2019-10-30T07:51:49+01:00"
title = "Python VS Common Lisp, workflow and ecosystem"
draft = false
+++

<!-- started = "2017-02-05T07:51:49+01:00" -->
<!-- finished = "2019-10-30" -->


I learned Java and C at school, I learned Python by myself and it was
a relief. After 8 years working and doing side projects in Python and
JavaScript (mostly web dev, Django/Flask/AngularJS/Vuejs), I am not
satisfied anymore by the overall experience so I'm making Common Lisp my
language of choice.

I am not here to compare languages themselves, but their inherent
workflow and their ecosystem. This is the article I wish I had read
earlier, when I was interested in Lisp but was a bit puzzled, because
the Lisp way always seemed different, and I couldn't find many
voices to explain it. The Python way may not be the
most practical or effective, Common Lisp might not be a dead language. I find many "workflow
fixes", overall improvements and hackerish possibilities on the CL side, even if sometimes the
Python tooling is superior.

Let's dive in.

and thanks to the proofreaders.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Development process](#development-process)
    - [Interactivity](#interactivity)
    - [Editing code](#editing-code)
    - [Running, testing programs](#running-testing-programs)
    - [Typing](#typing)
    - [Refactoring](#refactoring)
- [Libraries](#libraries)
    - [Library management](#library-management)
    - [State of the libraries](#state-of-the-libraries)
- [Templates](#templates)
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

In *Python*, we typically restart everything at each code change, we use
 breakpoints: this takes some time, I find it too repetitive and boring, it requires to
 re-manipulate data to re-reach the state we were at to analyze and
 debug our program. We might figure out a non-standard,
 more interactive way, but still: a web server needs to restart,
 object instances don't get updated after a class definition. We can get a prompt on an error (`-m pdb`), some tools include it (Werkzeug): a sign that it is a good thing to have. Unfortunately, it is not built-in, as in CL.

In *Common Lisp*, everything is much more interactive in the REPL. Even
developing web apps. On an error, we get an interactive debugger with
the stacktrace in our editor, we press `v` and voilà, we are at the
problematic line. We can of course catch errors to avoid the debugger, or disable it with global settings. We can resume the program execution from any stackframe. No process needs to restart. The variables
that we define on the REPL stay here. If we change a class definition
(say, we remove a field), existing instances get (lazily) updated.

The Lisp REPL is part of the development process, it is not only used
for exploration and debugging. It's fun, it's a productive boost, and
it allows to catch errors earlier, both because we try functions
earlier, and because we get type warnings when we compile the file or
the current function (yes, we can compile a single function).

Now, the cost is that one must learn to play with this live data. We
might come to a state that doesn't reflect the code anymore, so we'll
write our own "reset" functions or just restart the lisp image.

Here's a video where the developer defines a dummy interface, makes it
fail, develops it, and tests it, all quickly by interacting with the REPL.

<iframe width="560" height="315" src="https://www.youtube.com/embed/CNFr7zIfyeM" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

## Editing code

*Python*: we edit code line by line, paragraph by paragraph. We can
try out half-backed editor plugins to edit code by semantic units. Sometimes we
must even pay attention to add a couple whitespace there, remove one
there. We are far from the immediate interactive feedback of the hacker's
vision "Inventing on Principles".

*Common Lisp*: we edit code by semantic units. I love emacs' [lispy
mode](http://oremacs.com/lispy/), which is weird at first of course,
but so convenient. We can navigate to expressions back and forth, we
can delete a whole "if" expression with a keypress, indentation is
automatic, etc. There are [other emacs plugins](http://wikemacs.org/wiki/Lisp_editing). [Parinfer](https://shaunlebron.github.io/parinfer/) is appreciated in other editors too.

Actually, we edit code by parenthesis units, which doesn't carry as
much meaning as an Abstract Syntax Tree. For a real AST, we'd need a code walker. But
since Lisp's syntax is based on parenthesis, in practice the
experience is similar.

I had a try on writing a little plugin to help editing Python code by manipulating
the AST ([red4e](https://github.com/vindarel/redbaron4emacs)).  We
first need an AST parser. There was a couple for Python 2, another one
for Python 3 without type annotations, eventually one emerged a couple
years later: these are signs of an unstable language and ecosystem, and it is more work
required by the developer. I went the simple way by calling each
function into a new Python process, which is of course too
slow. [traad](https://github.com/abingham/traad) is a better project,
it can do much more but still, it's difficult to answer questions like
cross-referencing: "who calls this function" or "who does this
function call", which are built-in in
SLIME. [SLIME](https://en.wikipedia.org/wiki/SLIME) is like the Language
Server Protocol for Common Lisp in Emacs, its backend Swank being
editor-agnostic.

Maybe other editors and proprietary ones come with a better
experience, at the cost of freedom, money, configuration time and
memory and CPU resources. If I have the choice, I prefer to not go
this route, and choose a better platform from the start.

Traad is built around a client-http server approach, this is the idea
behind LSP…  this reminds me of the architecture of SLIME!
It has a backend, Swank, and a client (SLIME for Emacs, SLIMA for Atom,…).
It thus has a modern architecture since its
inception :) It is moreover based on a stable language whose syntax
can not rot and has decades of development behind it, so we can be
confident about the tool. Saying this because it's hard to grasp what
SLIME is at the beginning.

SLIME itself is tied to Emacs, and thus a newcomer can find the UI
unpractical. Swank though can be used outside of Emacs, and it is for
example for Atom's [SLIMA](https://github.com/neil-lindquist/SLIMA/),
which now has all the most important SLIME features: REPL, integrated
debugger, jump to definition, autocompletion, interactive object
inspection, and more.

- more: https://lispcookbook.github.io/cl-cookbook/editor-support.html (Eclipse, Lem, Jupyter Notebook,…)

[![](https://raw.githubusercontent.com/tarsius/paren-face/master/parentheses.png)](https://github.com/tarsius/paren-face/)


## Running, testing programs

*Python*: the default workflow is to run commands in the
terminal. Scroll, read the output, copy-paste manually (or use the
non-UX-optimal termux or a terminal inside emacs), go back to your
editor. Type commands without completion, type the whole path to a
single unit test (`pytest path/to/test.py::foo`), or configure your
editor and find a good plugin that is compatible with your test runner (I
can't use the excellent nose-mode :( ).

*Common Lisp*: the default workflow is to do everything interactively
into the REPL, but some people still use a
write-compile-run approach. Consequently there is built-in completion for
everything. We don't have to use the shell (except from once in a
while to run global tests or build the system) and that's a good
thing. There is an interactive debugger. We can interactively fix and re-run code and
tests.

Here's a quick demo on how to interactively fix failing tests:

<iframe width="560" height="315" sandbox="allow-same-origin allow-scripts" src="https://peertube.video/videos/embed/c0c82209-feaa-444d-962f-afa25745bfc0" frameborder="0" allowfullscreen></iframe>

Running and debugging on a remote server: in Python, we usually rsync
sources and run tests manually, or start vim/emacs under tmux on the
server. We have to kill the app to reload it. In Common Lisp, we can
connect to the running, remote instance, write changes from the
comfort of our editor locally, hit `C-c C-c` on a function to compile it
and see changes on the remote image. CL has more hackerish capacities
here, no doubt, and I find it attractive :)

- more information on (remote) debugging: https://lispcookbook.github.io/cl-cookbook/debugging.html
- watch Baggers working with OpenGL: https://www.youtube.com/watch?v=a2tTpjGOhjw&index=20&list=RDxzTH_ZqaFKI
- a Minecraft game engine that you can change while playing: https://github.com/gmasching/sucle

## Typing

*Python*: we catch a lot of type errors in production, and/or we have
to write a lot more unit tests. Hope we agree on this.

Now we can improve the situation somehow with type annotations,
however it has the cons of being an after-thought: it is not stable
(differences between Python versions), not well integrated
(we have to run another command, choose between mypy, the new typing
module, pyre), it is not interactive, we need to configure our IDE, it
adds a start-up penalty (which might or might not be important).

In *Common Lisp*, particularly with SBCL, we get a lot of type errors
or warnings at compile time. We can compile *a single function*, and
thus have an immediate feedback.  We're closer (not there, just
closer) to the "if it compiles, it works" situation (we know it runs,
since we constantly compile and try the functions). We can also create
our compound types and add type declarations to variables and
functions. It's great, though it doesn't do as much static checks as a
real typed language.

Adding type declarations in well chosen places such as inner loops
also allows to gradually speed up the program where needed.

- https://lispcookbook.github.io/cl-cookbook/type.html
- [Compile-time type checking in the programmable programming language Lisp](https://medium.com/@MartinCracauer/static-type-checking-in-the-programmable-programming-language-lisp-79bb79eb068a)
- will [ML embedded into CL](https://github.com/tarballs-are-good/coalton)
help even more ?

## Refactoring

*Python*: we can't refactor code as we want. Decorators, context
managers: they have an interface and they are limited to what they
offer. You can't do things a bit differently, you must comply to the
interface. That might be a feature, but I prefer not being
restricted. In my experience, this leads to code repetition whereas
in CL, we can refactor how we want, and we get a cleaner code.

*Common Lisp*: there are similar patterns than in Python, but we can
escape them. We can use macros, be concise and do what we want. We can
have the decorator syntax with the cl-annot library, and any other by
writing our reader macros (they can bring triply-quoted docstrings,
string interpolation, infix notation, C syntax…). It's not only macros
though. The polymorphism of the object system (or generic dispatch)
helps, and Lisp's "moldability" in a whole allows us to refactor code
exactly how we want, to build a "Domain Specific Language" to express
what we want. Other language features than macros help here, like
closures or [multiple
values](https://lispcookbook.github.io/cl-cookbook/functions.html#multiple-return-values-values-multiple-value-bind-and-nth-value)
(which are different, and safer for refactoring, than returning a tuple).

Now, speaking about refactoring tools, they are better Python side. I
don't know of a Lisp tool that allows to change all the code-base
according to the AST, maybe in a proprietary editor. There are
utilities to make local transformations, like "extract this expression
into a `let` variable at the top of the function", "transform a
function to a lambda equivalent" or the contrary, etc.

# Libraries

## Library management

*pip*: use virtual environments (virtualenv, virtualenvwrapper, tox,
anaconda,…  or install per-user), pin dependencies (pip-tools, pipenv, poetry,
pyupdate,…). Debug problems due to
a third party library that didn't pin its dependencies strictly
enough (happens at the wrong moment).

*quicklisp*: think of it like Debian's apt, shipping releases that
work together (that load together), and that we upgrade together, when we
want. If needed, we can still clone projects into
`~/quicklisp/local-projects/` for a system-wide installation, or have
project-local dependencies with
[Qlot](https://github.com/fukamachi/qlot).

We are not even limited to Quicklisp any more (it can be limiting
because of its one month release cycle). The
[Ultralisp](http://ultralisp.org/) distribution builds every 5
minutes. [clpm](https://gitlab.common-lisp.net/clpm/clpm) is a package
manager with a traditional approach. One can publish his own Quicklisp
distribution, to provide a set of packages that are known to work
together.


## State of the libraries

CL might have more libraries than you think, see the [Awesome CL
list](https://github.com/CodyReichert/awesome-cl),
http://quickdocs.org/ or do a quick search on the net. I know I am
constantly surprised.

But sure, the Python ecosystem is huge. A few remarks on the differences:

- Quicklisp has around 1500 packages, PyPI over than 170 000. It's hard to imagine that there are a hundred times more useful libraries :D Even in CL we have duplication of libraries with a dozen of test frameworks.
- Quicklisp is a curated distribution, PyPI is not. That means that
  libraries that don't compile anymore are rejected (after a notice to
  the maintainers), and that orphan projects' URL can be updated to
  point to a community maintained one.
- Anybody can easily publish a library to PyPI on its own. Less so
  with Quicklisp, one must open an issue (Ultralisp doesn't have this limitation).
- [numcl](https://github.com/numcl/numcl) is a Numpy clone.
- if needed, you can use [py4cl and more](https://github.com/CodyReichert/awesome-cl#python) to interface with Python.

An important remark, is that Common Lisp is a stable language, and
that the libraries play this game (I saw a deprecation feature staying
for 12 years). We can still run code that was written in the early 90's.

Lisp's simpler, non-rotting syntax plays a good role on
stability. Caution: that doesn't mean the implementations don't
evolve, quite the contrary.

In his appreciated article [A Road to Common Lisp](http://stevelosh.com/blog/2018/08/a-road-to-common-lisp/), the author writes:

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

You might use Mako templates, but there's something you can't do.

In CL, we can also use a Django-like templating engine, [Djula](https://github.com/mmontone/djula)
templates (despite its modest number
of stars, it is one of the most downloaded projects on Quicklisp).
The Mako equivalent would be [Eco](https://github.com/eudoxia0/eco).
However, we can alternatively just use plain old Lisp, for example with
[Spinneret](https://github.com/ruricolist/spinneret/). As a
consequence, we can factorize code as we always do (with spinneret functions or
lisp macros). We manipulate code as we always do. It even warns on
malformed html and has some neat features (it is clever about headers
levels, it can embed markdown, etc).

Stuff like this is less possible with Python, because the language is
less flexible. The components libraries I have seen use strings inside
Python code.

# Deployment, Shipping

Shipping an app, even more a web app, in *Python* (and JS) is
tedious. There are no default way to ship a self-contained
executable. Current projects aiming at fixing that can work… and may not.

So the current solution is to turn to containers. They're the Big
Thing, but we still need to spend hours on reading resources, building
the Docker file, the deployment pipeline, fixing bugs, updating the
stack, accepting security holes, etc. Hours we could put on our
app. With Docker though, users still can't download a binary.

In *Common Lisp*: we (re)discover the joy of a compiled language. We
compile our program to machine code, the binary embeds the run-time, the
debugger, the web server, the static assets, and we ship it. We run it on
the server and we can access it from the outside straight away.

An SBCL image of a non-trivial web project will weight ± 20 to 30MB
(with core compression). For a lighter binary (not that I care
personally), we could try ECL (that compiles to C), or use
tree-shakers of proprietary implementations (LispWorks, Allegro).

We can still benefit from Docker if needed, of course.

*Deployment process in Python*: install Python and pip, install pip
dependencies and their system requirements and be prepared for errors (or try non-standard tools,
like Platter), configure a server for static files (nginx, whitenoise), run a
WSGI web server,…

*Deployment in CL*: build your binary, send it to the server, run
it. Configure nginx eventually. We can compile and include assets into
the image (see [Rock](https://github.com/eudoxia0/rock)).

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
- [pgloader](https://tapoueh.org/blog/2014/05/why-is-pgloader-so-much-faster/) was re-written from Python to Common Lisp for a 30x speed gain.


# Conclusion

I hope I killed some FUD and showed you new ways to make stuff. May
that inspire you!


# Appendix A: FAQ

Some info every Python programmer will come across eventually. Saves you some googling.

## Are there no iterators ?

In practice, we mostly rely on closures, but there are libraries to
create iterators.

See https://stackoverflow.com/questions/32956033/is-there-a-straightforward-lisp-equivalent-of-pythons-generators

## Can I define my own `+` operator like in an OO language ?

By default, no, because the Common Lisp Object System (CLOS) came after the language specification
and thus everything isn't object-based. However there are libraries
like [generic-cl](https://github.com/alex-gutev/generic-cl/) and, in
practice, we quickly forget about this. Different operators is also a
means for performance, good type inference and error messages.

## To which extent can Lisp be compiled, with all its dynamic nature, garbage collection, macros and what else ?

Many Lisp compilers compile to machine code (SBCL, CCL, CMUCL,…).

Full answer: https://stackoverflow.com/questions/913671/are-there-lisp-native-code-compilers/914383#914383


## But what is Common Lisp good for, really ?

We have a ready-to-use citation :)

> Please don't assume Lisp is only useful for Animation and Graphics, AI, Bio-informatics, B2B and Ecommerce, Data Mining, EDA/Semiconductor applications, Expert Systems, Finance, Intelligent Agents, Knowledge Management, Mechanical CAD, Modeling and Simulation, Natural Language, Optimization, Research, Risk Analysis, Scheduling, Telecom, and Web Authoring just because these are the only things they happened to list. -- Kent Pitman

Kent Pitman

http://www.nhplace.com/kent/quoted.html

See also http://random-state.net/features-of-common-lisp.html


## So why is CL not more popular ?

First, some reminders:

- popularity doesn't equal quality, and popularity is hard to
  measure.
- some success stories: http://lisp-lang.org/success/ Aircraft analysis suits, Missile defense, ICAD, music composition, algebra systems, bulk importer for PostgreSQL, grammar checking, 3D editor, knowledge graphs,…
- did you know that [pgloader](https://tapoueh.org/blog/2014/05/why-is-pgloader-so-much-faster/) was re-written from Python to Common Lisp? (for a x30 speed gain, among other benefits)
- CL was [used in a spacecraft](https://www.youtube.com/watch?v=_gZK0tW8EhQ&feature=youtu.be&t=4175) (and the REPL was used to debug the system live from the earth)
- some companies still use and pick CL: https://github.com/azzamsa/awesome-lisp-companies, companies provide professional support ([Platform.sh](https://platform.sh/)).
- Google's [ITA Software](https://en.wikipedia.org/wiki/ITA_Software) still powers airfare search on Orbitz or Kaya.com,
- reddit v1 was written in CL! JavaScript was written in CL!
- CL was number 2 on the Tiobe index for years in the 80s!

That being said, my 2 cents since you ask:

- I think the CL world missed the web bandwagon for some time ([common-lisp.net](http://common-lisp.net/) was horrible for some years), but that's being fixed.
- an enormous code-base existed before GitHub.
- we missed visually nice, practical content on the web, even though
  there are many books. It's a bit better now.
- CL missed a package manager for some time behind other languages, that's now fixed.
- I reckon CL is still quite hard for the web, it doesn't have a killer web framework (though maybe [Weblocks](http://40ants.com/weblocks/quickstart.html) soon©, an isomorphic web framework), hence no hype.
- CL seems to be used for big, non-trivial projects, hence it gets no easy hype.
- CL has no entity doing marketing today. We saw the Common Lisp
  foundation pairing with sponsors recently. It *did* receive a lot of
  financial and institutional support from the MIT, the NASA, Xerox, Carnegie
  Mellon University (CMUCL), Lisp vendors (Symbolics, Lucid, Franz…),…
- CL worked well with Emacs, Vim, CCL's built-in editor on macOs,
  LispWorks' editor (which has a free version), but this doesn't
  satisfy the masses. We now have more options, including Atom (very
  good support) and Eclipse (basic support).
- other reasons: it may be hard (or harder than the concurrence) to
  grasp and getting started with, Lisp isn't for everyone, it gets a lot of
  FUD, and has a so-called Lisp curse!

but that's all debatable, I wouldn't focus much on this. [Times are
good for
implementations](https://lisp-journey.gitlab.io/blog/these-years-in-common-lisp-2018/#implementations), that's what counts.
