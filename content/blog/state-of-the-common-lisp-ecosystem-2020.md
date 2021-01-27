---
title: "State of the Common Lisp ecosystem, 2020"
date: 2021-01-27T16:29:11+02:00
draft: true
---

DRAFT

This is a description of the Common Lisp ecosystem, as of January, 2021,
from the perspective of a user and contributor.

The purpose of this article is both to give an overview of the
ecosystem, and to help drive consolidation in each domain.

Each application domain has recommendations for consolidating that
part of the ecosystem, and pointers for interesting future work.

This article is derived from
Fernando Borretti's [State of the Common Lisp ecosystem from 2015](https://borretti.me/article/common-lisp-sotu-2015).
This new one will be an opportunity to look at what was achieved -or what is
still lacking.

these years… 2018

More libraries can be discovered on the [Awesome-cl](https://github.com/CodyReichert/awesome-cl) list -and on GitHub. - cl-net libraries page


# Application domains

## Command line

There used to be several options, but now
[Roswell](https://github.com/snmsts/roswell) has gained most momentum,
and that's a good thing. Roswell is an implementation
manager/installer and script runner. One neat feature is support for
very easily compiling tiny scripts into executables.

[cl-readline](https://github.com/vindarel/cl-readline) and
[linedit](https://common-lisp.net/project/linedit) are still there.

To parse command line arguments, [unix-opts](https://github.com/mrkkrp/unix-opts) shows a decent activity.

[Adams](https://github.com/cl-adams/adams) is a new UNIX system administration tool, not unlike Chef or Ansible.

**Consolidation**

More features to the [sripting libraries](https://github.com/CodyReichert/awesome-cl#scripting) is necessary.

## Databases

[Mito](https://github.com/fukamachi/mito) is an ORM for Common Lisp
with migrations, relationships and PostgreSQL support. It is based on
cl-dbi (a uniform interface to the various database server-specific
libraries such as cl-postgres and cl-mysql) and SxQL (a DSL for
building safe, automatically parameterized SQL queries).

It also has a tutorial in the Cookbook:
[Cookbook/databases](https://lispcookbook.github.io/cl-cookbook/databases.html).

<!-- On my blog, [an article](https://lisp-journey.gitlab.io/blog/composing-queries-with-mito-aka-replacing-lazy-querysets-and-q-objects/) -->
<!-- on how to compose queries with Mito and SxQL, and on how we only need -->
<!-- lisp knowledge to replace Django functionalities. -->

There are of course many more libraries. Some new ones since 2015 are:

[cl-yesql](https://github.com/ruricolist/cl-yesql) (by the author of
Serapeum, Spinneret and other great libraries) is based on Clojure's
Yesql.

[vivace-graph](https://github.com/kraison/vivace-graph-v3) is a **graph
database** and Prolog implementation, taking design and inspiration from
CouchDB, neo4j and AllegroGraph.

Vsevolod Dyomkin, the author of Rutils, the Programming Algorithms
book and other libraries, is writing
[cl-agraph](https://github.com/vseloved/cl-agraph), a minimal client
to Franz Inc's [AllegroGraph](https://allegrograph.com/). AllegroGraph is a
"horizontally distributed, multi-model (document and graph),
entity-event **knowledge graph** technology". It is proprietary and has a
free version with a limit of 5 million triples.

A general migration tool was lacking. We now have
[cl-migratum](https://github.com/dnaeon/cl-migratum), a "system which
provides facilities for performing database schema migrations,
designed to work with various databases".

And of course, [pgloader](https://github.com/dimitri/pgloader) is still a Common Lisp success story.

**Achievement**

Among the emerging ORMs, Mito is the one actively maintained that lispers seem to have chosen. Good.

CLSQL certainly still works, but we don't hear about it and it looks outdated. So, Mito it is.

**Consolidation**

Mito has 11 contributors and is actively watched, but it probably should have another(s) core maintainers.

**Future work**

Bindings for the new databases coming out.

## Datastructures

## Concurrency

Many libraries exist in this area:

* [BordeauxThreads](https://common-lisp.net/project/bordeaux-threads/) - Portable, shared-state concurrency
  - the "de-facto" concurrency library.
* [lparallel](https://github.com/lmj/lparallel) - A library for parallel programming.
  - also solid, battle-tested and popular, aka de-facto.
* [calispel](https://github.com/hawkir/calispel) - [CSP](https://en.wikipedia.org/wiki/Communicating_sequential_processes)-like channels for common lisp. With blocking, optionally buffered channels and a "CSP select" statement. ISC-style.
  - "It is complete, flexible and easy to use. I would recommend Calispel over Lparallel and ChanL." @Ambrevar. [discussion](https://github.com/CodyReichert/awesome-cl/issues/290)
* [ChanL](https://github.com/zkat/chanl) - Portable, channel-based concurrency.
* [cl-async](https://github.com/orthecreedence/cl-async) - A library for general-purpose, non-blocking programming.
  * works atop libuv
* [Moira](https://github.com/TBRSS/moira) -  Monitor and restart background threads. In-lisp process supervisor.
* [trivial-monitored-thread](https://gitlab.com/ediethelm/trivial-monitored-thread) -
  a Common Lisp library offering a way of spawning threads and being
  informed when one any of them crash and die.
* [lfarm](https://github.com/lmj/lfarm) - distributing work across machines (on top of lparallel and usocket).
* [cl-gearman](https://github.com/taksatou/cl-gearman) - a library for the [Gearman](http://gearman.org/) distributed job system.
  * Alexander Artemenko used it instead of lfarm for Ultralisp:
    https://40ants.com/lisp-project-of-the-day/2020/06/0095-cl-gearman.html,
    because "lfarm is not well suited to environments where worker
    hosts can go down and return back later".
* [swank-crew](https://github.com/brown/swank-crew) - distributed computation framework implemented using Swank Client.
* [cl-coroutine](https://github.com/takagi/cl-coroutine) - a coroutine library. It uses the CL-CONT continuations library in its implementation.

(see [awesome-cl#parallelism-and-concurrency](https://github.com/CodyReichert/awesome-cl#parallelism-and-concurrency))

In the last year, Manfred Bergmann developed
[cl-gserver](https://github.com/mdbergmann/cl-gserver). It is a
"message passing" library/framework with **actors** similar to
**Erlang** or **Akka**. We think it is an important achievement.

Its version 1 features:

- actors can use a shared pool of message dispatchers which effectively allows to create millions of actors.
- the possibility to create actor hierarchies. An actor can have child actors. An actor now can also "watch" another actor to get notified about it’s termination.

Also, we (re)discovered [CMTX](https://github.com/cosmos72/stmx): high
performance transactional memory for Common Lisp ). In our opinion, a
library not well known and under-appreciated.

**Consolidation**

There is some choice paralysis between Lparallel, Calispel and
Bordeaux-Threads.

Use the libraries in the wild and write about it.


## File formats

There exist Common Lisp libraries for all the major file formats:

- XML: [Plump](https://github.com/Shinmera/plump) (and [Lquery](https://github.com/Shinmera/lquery/))
- JSON: [Jonathan](https://github.com/Rudolph-Miller/jonathan), [cl-json](https://common-lisp.net/project/cl-json/) or [more](https://sabracrolleton.github.io/json-review).
- YAML: cl-yaml
- CSV: [cl-csv](https://github.com/AccelerationNet/cl-csv)

Jonathan was new in 2015. It was presented as "a very fast JSON encoder and decoder".

To work with JSON, we now have:

- [json-pointer](https://github.com/y2q-actionman/cl-json-pointer) - A JSON Pointer implementation.
- [json-mop](https://github.com/gschjetne/json-mop) - A metaclass for bridging CLOS and JSON objects (remind that JSON libraries can already serialize your own objects).
- [json-schema](https://github.com/fisxoj/json-schema)


**Consolidation**

There are still too many JSON libraries. This leads to choice paralysis.

They all represent null values differently. We need a library that
"does the right thing". See maybe the massive [web-toolkit](https://github.com/xh4/web-toolkit#json) for its JSON handling ?

> It distinguishes null, false and [] from Lisp's NIL thus supports identical transformation between JSON values. It provides object constructor and accessor to build and access nesting JSON objects.

Help maintain cl-csv.

**Future Work**

Still valid from 2015:

> A YAML parser so that cl-yaml doesn’t depend on the libyaml library would make distribution far simpler.


## GUI

A usual complain in Common Lisp land is the lack of a complete,
cross-platform GUI solution. Ltk is a very good library, but Tk is
limited. Qtools is great, but is only for Qt4.

A lot has happened, and is still happening (if you watch the right
repositories, you know that a Qt5 wrapper is in the works).

Matthew Kennedy wrote excellent FFI bindings to the IUP Portable User
Interface library: [IUP](https://github.com/lispnik/iup/). IUP is
cross-platform (Windows, macOS, GNU/Linux, with new Android, iOs,
Cocoa and Web Assembly drivers), has many widgets (but less than Qt), has a small api and
is actively developed. IUP was created at the PUC university of Rio de Janeiro.

Pavel Korolev develops bindings to the Nuklear immediate-mode library:
[bodge-nuklear](https://github.com/borodust/bodge-nuklear) (and he
uses it in his games which you should check out too).

Nicolas Hafner started [Alloy](https://github.com/Shirakumo/alloy), a
new user interface protocol and toolkit implementation, which he uses in his Kandria game.

Very recently, David Botton released [CLOG](https://github.com/rabbibotton/clog), "the Common Lisp Omnificent GUI":

> CLOG uses web technology to produce graphical user interfaces for applications locally or remotely. CLOG can take the place, or work alongside, most cross-platform GUI frameworks and website frameworks. The CLOG package starts up the connectivity to the browser or other websocket client (often a browser embedded in a native template application.)

It is "complete enough for most uses". You might find a demo [here](http://office.botton.com:8080/).

There are more GUI libraries and frameworks: https://github.com/CodyReichert/awesome-cl#Gui (and more under the works).

**Consolidation**

Since roughly October, 2020, Nicolas Hafner works full time on
[Kandria](https://kandria.com/). Supporting his work, through [GitHub
sponsors](https://github.com/sponsors/Shinmera) or
[ko-fi](https://ko-fi.com/shinmera) would be 1) a great sign of recognition and 2) useful for ecosystem, especially for Alloy.

I wrote an introduction to these frameworks in the Cookbook:
[Cookbook/gui](https://lispcookbook.github.io/cl-cookbook/gui.html). More
examples or demo projects would be welcome.

There are two actively maintained diverged forks of the Gtk bindings. A reunification effort is required.

**Future work**

Write a desktop application with IUP/your toolkit of choice for everydays' use and make it a Common Lisp flagship.

Study other approaches to GUI bindings. What about
[gtk-server](http://www.gtk-server.org/)? GObject introspection? An
effort started for Qt: [giqt](https://github.com/mrosset/giqt/) (in
which we recognize @ambrevar from the [Nyxt
project](https://github.com/atlas-engineer/nyxt/): supporting them is
helping the Lisp ecosystem too).

LispWorks' [CAPI](http://www.lispworks.com/products/capi.html) and Allegro's [Common Graphics](https://franz.com/products/allegro-common-lisp/acl_ide.lhtml)
are proprietary, but have free trial versions and are the most
advanced GUI toolkits for Common Lisp. CAPI even targets the Android platform. More examples and tutorials are necessary.

## Machine Learning

- [CLML](https://github.com/mmaul/clml), developed at Mathematical Systems Inc., a Japanese company.

## System

To quote Fernando again:

> UIOP, ASDF’s portable compatibility layer, contains a large set of tools for portably doing everything from querying the hostname to running external programs to manipulating environment variables.

We should not require cl-fad nor Osicat anymore.

Built on top of UIOP, Ruricolist's [cmd](https://github.com/ruricolist/cmd) brings short and handy commands to run and pipe programs.

## Web Development

### Backend

Common Lisp's main web servers are Hunchentoot and Clack. Since 2015, Clack's state of the documentation barely improved and is still lacking.

[Clack](https://github.com/fukamachi/clack), the equivalent of
WSGI/Rack has existed since 2009. Web frameworks are built on top of
it, for example [Caveman2](http://8arrow.org/caveman/). It is an HTTP
server abstraction, that allows the user to write web applications
(or, more reasonably, web application frameworks) without depending on
a particular server.

Ferando wrote:

> the importance of using Clack cannot be understated: If you build an application directly on, say, Hunchentoot, you’re tied to Hunchentoot, and if a new, faster server – like [Woo](https://github.com/fukamachi/woo) – comes out, you have to rewrite the entire application to use it. If you write a plugin for Clack – like [clack-errors](https://github.com/eudoxia0/clack-errors) – it is automatically usable by all applications, regardless of framework, that are built on Clack, reducing useless duplication of code.

> With Clack, switching from Hunchentoot to Woo, and enjoying the incredible speedup, is a simple matter of installing libev and changing a keyword argument.

This still holds true, but the situation didn't improve much. In comparison, Hunchentoot is very well documented (and you can read its documentation on [readthedocs here](https://common-lisp-libraries.readthedocs.io/hunchentoot/)), and it is "fast enough".

About Hunchentoot: Mariano Montone wrote [easy-routes](https://github.com/mmontone/easy-routes), a little but handy route handling facility on top of Hunchentoot. It brings dispatch by HTTP method, arguments extraction from the URL path, and "decorators". It is also integrated with the Djula framework to generate URLs from the route name.

**Achievement**

More [Clack plugins](https://github.com/CodyReichert/awesome-cl#clack-plugins)
were written, such as a single-sign on middleware.

**Consolidation**

Write more documentation for Clack. While lispers know about it,
they don't necessarily adopt it because of the lack of
documentation. We can expand this [getting started
guide](https://jasom.github.io/clack-tutorial/posts/getting-started-with-clack/).


### Frontend

Many HTML generators and template libraries exist (see the list below). However, some new and good ones appeared lately:

* [TEN](https://github.com/mmontone/ten), by Djula's maintainer, brings the completness of Djula with the usability of Eco (by Fernando Borretti), aka: you write Django-like HTML templates but you can interleave any Lisp code.
* [markup](https://github.com/moderninterpreters/markup) - a JSX-like templating engine, where HTML tags are Common Lisp code. Comes with an Emacs package.

A very new web framework appeared:

* [ISSR](https://github.com/interactive-ssr), for Interactive Server-Side rendering. It links a client to its server with a websocket connection, and it allows to **write interactive web pages without writing any JavaScript at all**. Yes, it competes in Weblocks' pace (and maybe, in CLOG's too). It is thus not unlike Phoenix's LiveView or [Hotwire](https://github.com/hotwired/turbo).

Other HTML generators and templating engines include:

* [spinneret](https://github.com/ruricolist/spinneret) - Common Lisp HTML5 generator.
* [cl-who](http://weitz.de/cl-who/) - The venerable HTML generator.
* [Djula](https://github.com/mmontone/djula) - A port of Django's template engine to Common Lisp.
* [cl-closure-template](https://github.com/archimag/cl-closure-template) - Implementation of Google's Closure templates. [LLGPL][8].
* [clip](https://shinmera.github.io/clip) - An HTML template processor where the templates are written in HTML.


### JavaScript

The two "historical" solutions are:

* [Parenscript](https://github.com/vsedach/Parenscript), a DSL that compiles a subset of Common Lisp to idiomatic JavaScript, and
* [JSCL](https://github.com/davazp/jscl), a CL-to-JS compiler designed to be self-hosting from day one. JSCL is not complete (yet), it lacks CLOS, format and loop.

Two new are in development:

* [Valtan](https://github.com/cxxxr/valtan), a CL to JS compiler.
* [JACL](https://tailrecursion.com/JACL/), JavaScript Assisted Common LispIt has a [recording from ELS 2020](https://www.youtube.com/watch?v=JYLlC_dgQ5w).

**Consolidation**

Help develop one of the existing CL-to-JS implementations. Why not have a look at JSCL's [issues
issues](https://github.com/jscl-project/jscl/issues)?

Bring some new macros to ParenScript for new JavaScript, as in [Paren6](https://github.com/BnMcGn/paren6/). For example, allow to write `async` and `await`.


# Development

## Implementations

## Editors

## Documentation

## Package Management

## Build System

## Type system

Quoting Fernando:

> There’s not much to say here, except that Common Lisp has a pretty great type system that is not exploited nearly enough.

And to our greatest pleasure, SBCL's type system continues to improve. For example, SBCL 1.5.9 now gives type warnings when a class's declared type doesn't match its `initform`. It continued to improve on SBCL 2.0 and onwards.

Moreover, the [Coalton](https://github.com/stylewarning/coalton/) library is bringing a dialect of ML on top of CL, in order to write **statically typed programs** similar in spirit to Standard ML, OCaml, and Haskell.

**Consolidation**

Help develop Coalton.


## Testing

## Debugging etc

flamegraph

# Community

## Online presence

new cl-net

cookbook

awesome-cl

readthedocs

new blogs

## New books

## Companies

## Developers to support

## Choice paralysis

helped by awesome-cl stars and +1

## Growth

# Last words
