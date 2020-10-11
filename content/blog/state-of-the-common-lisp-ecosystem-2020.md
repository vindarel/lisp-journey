---
title: "State of the Common Lisp ecosystem, 2020"
date: 2020-03-30T16:29:11+02:00
draft: true
---

DRAFT

This is a description of the Common Lisp ecosystem, as of November, 2020,
from the perspective of a user and contributor.

The purpose of this article is both to give an overview of the
ecosystem, and to help drive consolidation in each domain.

Each application domain has recommendations for consolidating that
part of the ecosystem, and pointers for interesting future work.

Acknowledgement: this article is derived (and sometimes, copied) from
Fernando Borretti's [State of the Common Lisp ecosystem from 2015](https://borretti.me/article/common-lisp-sotu-2015).
This new one will be an opportunity to look at what was achieved -or what is
still lacking.

these years… 2018

More libraries can be discovered on the [Awesome-cl](https://github.com/CodyReichert/awesome-cl) list -and on GitHub.


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

I wrote about it in the Cookbook:
[Cookbook/databases](https://lispcookbook.github.io/cl-cookbook/databases.html). On
my blog, [an article](https://lisp-journey.gitlab.io/blog/composing-queries-with-mito-aka-replacing-lazy-querysets-and-q-objects/)
on how to compose queries with Mito and SxQL, and on how we only need
lisp knowledge to replace Django functionalities.

There are of course many more libraries.

[cl-yesql](https://github.com/ruricolist/cl-yesql) (by the author of
Serapeum, Spinneret and other great libraries) is based on Clojure's
Yesql.

[vivace-graph](https://github.com/kraison/vivace-graph-v3) is a **graph
database** and Prolog implementation, taking design and inspiration from
CouchDB, neo4j and AllegroGraph.

Vsevolod Dyomkin, the author of Rutils, the Programming Algorithms
book and other libraries, is writing
[cl-agraph](https://github.com/vseloved/cl-agraph), a minimal client
for Franz Inc's [AllegroGraph](https://allegrograph.com/). AllegroGraph is a
horizontally distributed, multi-model (document and graph),
entity-event **knowledge graph** technology. It is proprietary and has a
free version with a limit of 5 million triples.

A general migration tool was lacking. We now have
[cl-migratum](https://github.com/dnaeon/cl-migratum), a system which
provides facilities for performing database schema migrations,
designed to work with various databases.

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

There is a lot in this area:

* [BordeauxThreads](https://common-lisp.net/project/bordeaux-threads/) - Portable, shared-state concurrency
  - the "de-facto" concurrency library.
* [lparallel](https://github.com/lmj/lparallel) - A library for parallel programming.
  - also solid, battle-tested and popular, aka de-facto.
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

[cl-gserver](https://github.com/mdbergmann/cl-gserver) is a new
library. It provides an Erlang-inspired GenServer. It is meant to
encapsulate state, but also to execute async operations. Also with
actors. Functionality regarding state is not unsimilar to Clojure's
Agent or cl-actors.

[CMTX](https://github.com/cosmos72/stmx) (high performance transactional memory for Common Lisp ) is still, IMO, not well known
and under-appreciated.

**Consolidation**

Blog posts, documentation, helping maintainers…

Porting Erlangen (CCL only) or common-lisp-actors (LispWorks only) to
other implementations.

## File formats


## GUI

A usual complain in Common Lisp land is the lack of a complete,
cross-platform GUI solution. Ltk is a very good library, but Tk is
limited. Qtools is great, but is only for Qt4.

A lot has happened, and is still happening (if you watch the right
repositories, you know that a Qt5 wrapper is in the works).

Matthew Kennedy wrote excellent FFI bindings to the IUP Portable User
Interface library: [IUP](https://github.com/lispnik/iup/). IUP is
cross-platform (Windows, macOS, GNU/Linux, with new Android, iOs,
Cocoa and Web Assembly drivers), has many widgets, has a small api and
is actively developed. IUP was created at the PUC university of Rio de Janeiro.

Pavel Korolev develops bindings to the Nuklear immediate-mode library:
[bodge-nuklear](https://github.com/borodust/bodge-nuklear) (and he
uses it in his games which you should check out too).

Nicolas Hafner started [Alloy](https://github.com/Shirakumo/alloy), a
new user interface protocol and toolkit implementation, which he uses in his Kandria game.

There are more: https://github.com/CodyReichert/awesome-cl#Gui

**Consolidation**

Since roughly October, 2020, Nicolas Hafner works full time on
[Kandria](https://kandria.com/). Supporting his work, through [GitHub
sponsors](https://github.com/sponsors/Shinmera) or [ko-fi](https://ko-fi.com/shinmera) is useful.

I wrote an introduction to these frameworks in the Cookbook:
[Cookbook/gui](https://lispcookbook.github.io/cl-cookbook/gui.html). More
examples or demo projects would be welcome.

There are two actively maintained diverged forks of the Gtk bindings. A reunification effort is required.

**Future work**

Write a desktop application with IUP for everydays' use and make it a Common Lisp flagship.

Study other approaches to GUI bindings. What about
[gtk-server](http://www.gtk-server.org/)? GObject introspection? An
effort started for Qt: [giqt](https://github.com/mrosset/giqt/) (in
which we recognize @ambrevar from the [Nyxt
project](https://github.com/atlas-engineer/nyxt/): supporting them is
helping the Lisp ecosystem).

LispWorks' [CAPI](http://www.lispworks.com/products/capi.html) and Allegro's [Common Graphics](https://franz.com/products/allegro-common-lisp/acl_ide.lhtml)
are proprietary, but have free trial versions and are the most
advanced GUI toolkits for Common Lisp. CAPI even targets the Android platform. Examples and tutorials would be welcome.

## Machine Learning

## System

uiop

## Web Development

### Backend

[Clack](https://github.com/fukamachi/clack), the equivalent of
WSGI/Rack has existed since 2009. Web frameworks are built on top of
it, for example [Caveman2](http://8arrow.org/caveman/).

It is an HTTP server abstraction, that allows the user to write web applications (or, more reasonably, web application frameworks) without depending on a particular server.

The importance of using Clack cannot be understated: If you build an
application directly on, say, Hunchentoot, you’re tied to Hunchentoot,
and if a new, faster server – like
[Woo](https://github.com/fukamachi/woo) – comes out, you have to
rewrite the entire application to use it. If you write a plugin for
Clack – like [clack-errors](https://github.com/eudoxia0/clack-errors) – it is automatically usable by all
applications, regardless of framework, that are built on Clack,
reducing useless duplication of code.

With Clack, switching from Hunchentoot to Woo, and enjoying the incredible speedup, is a simple matter of installing libev and changing a keyword argument.

**Achievement**

More [Clack plugins](https://github.com/CodyReichert/awesome-cl#clack-plugins)
were written, such as a single-sign on middleware.

**Consolidation**

Write more documentation for Clack. While lipers know about Clack,
they don't necessarily adopt it because of the lack of
documentation. We can expand this [getting started
guide](https://jasom.github.io/clack-tutorial/posts/getting-started-with-clack/).

### Frontend

This is bound by Common Lisp’s ability to compile to JavaScript.

The two "historical" solutions are
[Parenscript](https://github.com/vsedach/Parenscript), a DSL that
compiles a subset of Common Lisp to idiomatic JavaScript, and
[JSCL](https://github.com/davazp/jscl), a CL-to-JS compiler designed
to be self-hosting from day one. JSCL is not complete (yet), it lacks CLOS, format and loop.

Two new are in development:

* [Valtan](https://github.com/cxxxr/valtan), a CL to JS compiler.
* [JACL](https://tailrecursion.com/JACL/), JavaScript Assisted Common
  Lisp. It has a [recording from ELS
  2020](https://www.youtube.com/watch?v=JYLlC_dgQ5w).

**Consolidation**

The best way to help consolidation is to drive one of the existing
CL-to-JS implementations forward. Why not have a look at JSCL's
[issues issues](https://github.com/jscl-project/jscl/issues)?


# Development

## Implementations

## Editors

## Documentation

## Package Management

## Build System

## Type system

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
