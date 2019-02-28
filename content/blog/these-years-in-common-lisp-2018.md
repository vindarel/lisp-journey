---
title: "These Years in Common Lisp 2018"
date: 2019-02-28T14:42:46+01:00
draft: true
---

It's been already a little more than a year that I began my Lisp
journey. I made quaterly news digests, mainly from reddit's
feed:

- [Q1 2018](/blog/these-months-in-common-lisp-q1-2018/) - [Q2 2018](/blog/these-months-in-common-lisp-q2-2018/) - [Q3 2018](/blog/these-months-in-common-lisp-q3-2018/) - [Q4 2018](/blog/these-months-in-common-lisp-q4-2018/)

Time has come for a yearly overview ! What happened in the Common Lisp
world ? Are there (or groundbreaking promising useful fun) projects,
articles, discussions, tutorials ?

No need to say, I won't reference everything we find in the quaterly
posts, which don't list all new projects appearing on Quicklisp (we
can find these in the
[monthly Quicklisp releases](http://blog.quicklisp.org)) or Github.

I hope this overview will sharpen your interest on what is in my
opinion an under-sold and still very promising language and plateform,
that I happen to like more and more (and sooo more than Python ;) ).

Happy discoveries.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->

**Table of Contents**

- [Documentation](#documentation)
- [Implementations](#implementations)
- [Projects](#projects)
    - [New projects](#new-projects)
    - [Web](#web)
    - [GUI](#gui)
    - [Package management](#package-management)
    - [Deployment](#deployment)
    - [Music](#music)
    - [(re)Discoveries](#rediscoveries)
- [Articles](#articles)
- [Other screencasts](#other-screencasts)
- [Discussion](#discussion)
    - [Learning Lisp](#learning-lisp)
    - [Common Lisp VS ...](#common-lisp-vs-)

<!-- markdown-toc end -->


# Documentation

Common Lisp's online documentation could be more thorough and
welcoming. Fortunately, a few of us revived some projects and work on
it -my favourite project being the Common Lisp Coobook. This year, we
got tutorials on:

- [Datastructures](https://lispcookbook.github.io/cl-cookbook/data-structures.html)
- [Debugging](https://lispcookbook.github.io/cl-cookbook/debugging.html), including how to interactively debug a spacecraft,
- [the Common Lisp Object System (CLOS)](https://lispcookbook.github.io/cl-cookbook/clos.html)
- [Loop, iteration, mapping](https://lispcookbook.github.io/cl-cookbook/iteration.html)
- [Database access and persistence](https://lispcookbook.github.io/cl-cookbook/databases.html)
- [Error and condition handling](https://lispcookbook.github.io/cl-cookbook/error_handling.html)
- [Numbers](https://lispcookbook.github.io/cl-cookbook/numbers.html) and [multidimensional arrays](https://lispcookbook.github.io/cl-cookbook/arrays.html)
- [Scripting and building self-contained executables](https://lispcookbook.github.io/cl-cookbook/scripting.html)
- [Working with types](https://lispcookbook.github.io/cl-cookbook/type.html)

along with many improvements on other pages, like on [getting started](https://lispcookbook.github.io/cl-cookbook/getting-started.html) and [editor support](https://lispcookbook.github.io/cl-cookbook/editor-support.html).

Which brings me to it: the editors situation is much more open than
you think:

- The editor of choice is still **Emacs** with Slime (or
Sly),
- However, we can get started with Emacs and Lisp in 3 clicks with **Portacle**, a *self-contained batteries-included sbcl-included portable* Emacs tailored for CL,
- For **Vim** and **NeoVim** we have SLIMV, VLIME, and plugins can be written for NeoVim using [cl-neovim](https://github.com/adolenc/cl-neovim).
- Or if we want an editor written in  cl, there's the self-contained **Lem** editor, which also works for Python, Go, Rust, Nim, Scheme, HTML, JSX, along with a directory mode, an experimental LSP mode, calc-mode, and more,
- Not to forget that Mac Os X users can use the [Clozure Common Lisp IDE](https://ccl.clozure.com/docs/ccl.html#the-clozure-cl-ide)
- All editions of LispWorks (including the free) include the [LW IDE](http://www.lispworks.com/products/ide.html)
- For users of **Eclipse IDE**, there is the Dandelion plugin
- For popular editors, the experience is getting very good on **Atom** and the popular **Visual Studio Code** can be made to work with CL using [cl-lsp](https://github.com/cxxxr/cl-lsp).
- We have an **ipython-like REPL** (cl-repl),
- and for interactive notebooks, we have **Jupyter kernels** and yet another
notebook (Darkmatter).

A very welcome improvement is the Common Lisp fundation's website:
https://common-lisp.net/ It got a massive update and is now
attractive. We had http://lisp-lang.org/ (don't miss its success
stories section), but common-lisp.net was a googlers' honey pot.

This website uses two "awesome" lists that were created or massively
furnished last year:

- the [Awesome-CL](https://github.com/CodyReichert/awesome-cl) list,
  updated with hundreds of commits, which hopefully makes for a more
  discoverable and appealing ecosystem, and
- [Awesome Lisp Companies](https://github.com/azzamsa/awesome-lisp-companies):
  it was needed because Lispers didn't know a lot of companies using
  CL appart from IRobot, Google's ITA (powering
  [Kayak](http://kayak.com/), [Orbitz](http://orbitz.com/) and
  others), Grammatech, YCombinator, Siscog or other dead ones.


Other places to learn Common Lisp include:

- [cl-exercise: a Common Lisp Learning System running on browsers](https://www.reddit.com/r/lisp/comments/a9at82/clexercise_common_lisp_learning_system_running_on/)
- coding schools, like [Kattis](https://open.kattis.com/help)
- and competitive Programming websites like CodeForces,
   HackerEarth, HackerRank, and CodeChef.
- lastly, [Peter Norvig's book Paradigms of Artificial Intelligence Programming is available on Github](https://github.com/norvig/paip-lisp)

We also regularly have new screencasts to enjoy:

- a lot being from [Baggers](https://github.com/cbaggers/): he does the following and he streams live nearly weekly
  * [little bits of Lisp](https://www.youtube.com/playlist?list=PL2VAYZE_4wRJi_vgpjsH75kMhN4KsuzR_): short videos to learn Lisp basics
  * [lots of bits of Lisp](https://www.youtube.com/results?search_query=lots+of+bits+of+lisp+): long videos to dive deep in advanced subjects (macros, CFFI,…)
  * [Pushing pixels with Lisp](https://www.youtube.com/watch?v=82o5NeyZtvw&list=PL2VAYZE_4wRITJBv6saaKouj4sWSG1FcS): mostly working with OpenGL
  * and [more](https://www.youtube.com/user/CBaggers/playlists) !
- [Shinmera](https://www.youtube.com/playlist?list=PLkDl6Irujx9MtJPRRP5KBH40SGCenztPW)
  has lots of videos too, we can see him working on game engines,
  games, his libraries, Qt applications and more,
- the [CL study group](https://www.youtube.com/watch?v=z7V5BL6W3CA) (here, an introduction to Screamer, a non-deterministic programming library)


# Implementations

Time is good for Common Lisp implementations. Most date back from
decades and already proved what they can do (remember, SBCL is a
descendant of the Lisp that went to space). Hence the lack of hype,
IMO. Yet, many are in active development, and keep improving. As
`/u/defunkydrummer` observed:

> We are lucky to live in a time where Lisp development is still ongoing, many teams carrying the flag of open-source Lisp:

> - SBCL (new release today)
> - SICL (last commit 2 hours ago)
> - ECL (last commit, yesterday),
> - CLASP (last commit 2 days ago)
> - CCL (last commit 7 days ago),
> - CLISP (two weeks ago),
> - CMUCL (1 month ago)
> - ABCL (3 months ago)

SBCL has monthly releases. If you read the release notes, you might worry:

> the amount of changes in each release is decreasing these years

but, as `/u/baggers` notes:

> I think the commits tell a slightly different tale though. There is always a lot of background 'making stuff better' work than won't appear as the explanation would either be ultra internal and specific or would be super vague and very similar each month (for example 'stuff is slightly faster').
>
> For one that would be overly specific [this one](https://github.com/sbcl/sbcl/commit/adc83086ff26c46e647b22d76fe22d57889b6ace) might make for a good example. It's grand work, but doesn't surface in any specific lisp feature, stuff is just better.

Furthermore, a maintainer:

> Or the developers are too lazy to describe their changes.

which isn't a good reason ;)

We got a new release of Corman Lisp, a high performance Windows/32bit specific implementation with a built in IDE,

we have CLASP, targetting C++ through LLVM (see [“Lessons Learned Implementing Common Lisp with LLVM”](https://www.youtube.com/watch?v=mbdXeRBbgDM&app=desktop)), built with the Cleavir compiler, part of SICL, a very
new implementation of Common Lisp with fresh ideas,

we have ABCL targetting the JVM, Embedable Common Lisp, without
forgetting active commercial ones, like LispWorks and AllegroCL. While
I'm at it, you might want to have a look at
[MOCL](https://wukix.com/mocl) for IOs, Android and OSx.

We got a nice talk by Clozure Common Lisp's maintainer:
[this Old Lisp](http://thisoldlisp.com/) (this one may be the second
most used implementation, particularly good for development -- super fast
compilation times (I heard it compiles itself in seconds), [advising](https://ccl.clozure.com/manual/chapter4.3.html#Advising), [watched objects](https://ccl.clozure.com/manual/chapter4.12.html#watched-objects), its own [IDE](https://ccl.clozure.com/docs/ccl.html#the-clozure-cl-ide)).

Last note, a SBCL maintainer started a RISC-V port: [First RISCy Steps -- Porting SBCL to the RISC-V](http://christophe.rhodes.io/notes/blog/posts/2018/first_riscy_steps/)


So: welcome to this new world. It's bigger than I thought, for sure.


# Projects

I only list some projects that can be of interest to anybody. For the full stuff see the quaterly posts !

<!-- I fear that it would be seen as representative of the CL new tools and libraries.  -->

## New projects

- [Next browser 1.2.0 is out!](https://www.reddit.com/r/lisp/comments/a954yf/next_browser_120_is_out/): a browser exposing all its internals to CL. Be productive.
- [CANDO - A Computational Chemistry programming environment integrating Common Lisp and C++ based on the Jupyter notebook](https://hub.docker.com/r/drmeister/cando/)
- [Coalton, a dialect of ML embedded in Common Lisp (alpha)](https://github.com/tarballs-are-good/coalton)
- [Voxel game engine (Minecraft)](https://github.com/terminal625/sucle) - a Minecraft engine. Allows for interactive changes.
- [Emotiq - blockchain in Common Lisp](https://github.com/emotiq/emotiq)
- [Temperance - logic programming (in development, reached v1.0.0)](https://sjl.bitbucket.io/temperance/)
- [MAGICL: Matrix Algebra proGrams In Common Lisp - Rigetti Computing](https://github.com/rigetticomputing/magicl) (quantum computing)
- [SHCL: An Unholy Union of POSIX Shell and Common Lisp](https://github.com/bradleyjensen/shcl) ([reddit](https://www.reddit.com/r/lisp/comments/8kpbcz/shcl_an_unholy_union_of_posix_shell_and_common/))
- [JSCL 0.7.0 now supports CLOS thanks to the work of vlad-km](https://jscl-project.github.io/)
- [cl-torrents 0.9 - readline interface and 1337x.to scraper](cl-torrents 0.9 - readline interface and 1337x.to scraper) - a simple tool to search for torrents on popular trackers. My first CL app. Web and GUI interfaces in the making.
- [Introducing Seed: An Interactive Software Environment in Common Lisp](https://vimeo.com/237947324)
- [Tovero is a 3D modeling system for Common Lisp](https://common-lisp.net/project/tovero/)
- [RMSBolt: See what your compiler is going inside of Emacs (has minimal support for Common Lisp)](https://gitlab.com/jgkamat/rmsbolt)
- [pngload: A PNG (Portable Network Graphics) image format decoder](https://www.michaelfiano.com/projects/pngload/)
- [cl-vep: a video effects processor](https://www.reddit.com/r/lisp/comments/9brkej/clvep_a_video_effects_processor/)
- [algebraic-data-library](https://github.com/tarballs-are-good/algebraic-data-library/)
- [Petalisp](https://www.reddit.com/r/Common_Lisp/comments/8f6wez/petalisp_elegant_high_performance_computing/): Elegant High Performance Computing
- [wiki-lang-detect](https://github.com/vseloved/wiki-lang-detect): Text language identification using Wikipedia data
- [Dufy, a color library](https://github.com/privet-kitty/dufy)
- [ppath, a path manipulation library](https://www.reddit.com/r/Common_Lisp/comments/8gzm4w/ppath_a_path_manipulation_library/)
- [cl-statistics.lisp](http://compbio.ucdenver.edu/Hunter_lab/Hunter/cl-statistics.lisp)
- [Powerlisp: A simple tool to automate your work with dmenu/rofi](https://github.com/luksamuk/powerlisp)
- [json-mop](https://github.com/gschjetne/json-mop): A metaclass for bridging CLOS and JSON objects
- [clsh](https://github.com/obicons/clsh): a set of Lispy bindings for running and composing *nix processes
- [filtered-functions](https://github.com/pcostanza/filtered-functions) - enables the use of arbitrary predicates for selecting and applying methods.


## Web

- [Weblocks' new quickstart](http://40ants.com/weblocks/quickstart.html) -
Weblocks is an isomorphic web frameworks that allows to write
interactive web apps without writing Javascript (nor writing code that
transpiles to JS). It is seeing a massive update right now. Being Lisp,
we can build a self-contained executable of our web app, send it to
the server, run it and see it from the outside.
- [three email libraries](https://github.com/CodyReichert/awesome-cl#email)
- [reddit1.0 source code](https://github.com/reddit-archive/reddit1.0) ([comments](https://www.reddit.com/r/Common_Lisp/comments/886yeu/reddit10/)), then [Reddit's code runs on SBCL](https://www.reddit.com/r/Common_Lisp/comments/8ata3c/reddit_code_runs_on_sbcl/). See also [reddit](https://www.reddit.com/r/programming/comments/883vzs/old_reddit_source_code/).
- [Interactive Common Lisp code snippets in any web page](http://blog.klipse.tech/lisp/2018/05/07/blog-common-lisp.html)
- [arboreta-wasm - Common Lisp tooling for WebAssembly](https://github.com/Arboreta/arboreta-wasm)

For web libraries, see https://github.com/CodyReichert/awesome-cl#network-and-internet

## GUI

- [nodgui - yet another Tcl/Tk-based GUI package for Common Lisp](https://notabug.org/cage/nodgui) (based on Ltk, with syntax sugar and more meta-widgets)
- [IUP bindings GUI stuff](https://www.reddit.com/r/Common_Lisp/comments/au0dmv/more_iup_gui_stuff/) (in the works)
- [YstokWidgets Professional Edition](http://en.ystok.ru/products/ywidgets/)
- [MIDGETS - A collection of CAPI widgets and utilities](https://common-lisp.net/~loliveira/ediware/midgets/doc/)
- [subtext: A mostly-text-based UI bridges Common Lisp objects and runs of text. Minimal text-based user interface](https://github.com/stacksmith/subtext)
- [ftw: Common Lisp Win32 GUI library](https://github.com/fjames86/ftw)
- [Cocoa interface code written in Lisp for use with Clozure Common Lisp](https://github.com/plkrueger/CocoaInterface)
- [McCLIM 0.9.7 "Imbolc" release](https://common-lisp.net/project/mcclim/posts/McCLIM-097-Imbolc-release.html)
- [Demo SBCL script using Gtk](https://www.reddit.com/r/lisp/comments/a31oxr/demo_sbcl_script_using_gtk/)
- [Demo ABCL script using Java Swing](https://github.com/defunkydrummer/abcl-jazz)

for GUI libraries: https://github.com/CodyReichert/awesome-cl#gui

## Package management

Quicklisp is the de facto package manager, but new projects come to
complement it and bypass its limitations:

- [the second version of Ultralisp is available](http://40ants.com/posts/Second-version-of-Ultralisporg-is-available-now.html) - Ultralisp is an important project that fills a gap. It is a quicklisp distribution which updates every 5 minutes. It is also a Weblocks application!
- [quicksys](https://lisp.com.br/quicksys/) - installs systems from multiple Quicklisp distributions.

For more options, see [Qlot](https://github.com/fukamachi/qlot) (install and pin libraries locally, like
Python's virtualenv) and [Roswell](https://github.com/roswell/roswell/).


## Deployment

- [Apache Thrift gains CL support](https://github.com/apache/thrift/commits/master)
- [s2i-lisp: Common Lisp + Quicklisp OpenShift Build Image](https://github.com/hjudt/s2i-lisp)
- [lisp-images: Docker images for common lisp development](https://github.com/fisxoj/lisp-images) (with some others, see the awesome-list)
- [A docker container for CL development](https://hub.docker.com/r/eshamster/cl-devel2/) (also [lisp-devel](https://hub.docker.com/r/daewok/lisp-devel/), [CI on CL Cookbook](https://lispcookbook.github.io/cl-cookbook/testing.html#gitlab-ci))
- [Kubernetes Client Library for Common Lisp](https://github.com/xh4/cube)
- [Heroku buildpack for Common Lisp](https://gitlab.com/duncan-bayne/heroku-buildpack-common-lisp)
- [cl-aws-custom-runtime](https://github.com/y2q-actionman/cl-aws-custom-runtime-test) - An example of using Common Lisp (SBCL) as a custom runtime on AWS lambda.
- [prometheus.cl](https://github.com/deadtrickster/prometheus.cl) - Prometheus.io client. Grafana dashboard for SBCL and Hunchentoot metrics (memory, threads, requests per second,…).

We can also deploy apps on Digital Ocean, and no need to say that
deploying a
[self-contained executable](https://lispcookbook.github.io/cl-cookbook/scripting.html#building-a-self-contained-executable)
is easy,
[connecting to a remote instance](https://lispcookbook.github.io/cl-cookbook/debugging.html#remote-debugging)
too.


## Music

- [Music: Music framework for musical expression in Common Lisp with a focus on music theory (built from scratch, on development)](https://github.com/MegaLoler/Music)
- [Composing in Lisp with Csound](https://michaelgogins.tumblr.com/post/178126207468/composing-in-lisp)
- [Shuffletron, a Common Lisp Music Player for the terminal](https://lisp-journey.gitlab.io/blog/shuffletron-lisp-music-player-for-the-terminal/)

see also [audio and music composition software](https://github.com/CodyReichert/awesome-cl#audio)

## (re)Discoveries

- [lfarm - a library for distributing work across machines (on top of lparallel and usocket)](https://github.com/lmj/lfarm)
- [Screamer - nondeterministic programming. Augment Common Lisp with practically all of the functionality of both Prolog and constraint logic programming languages (10 yo, Nikodemus)](https://github.com/nikodemus/screamer)
- [quid-pro-quo: a contract programming library in the style of Eiffel’s Design by Contract](https://github.com/sellout/quid-pro-quo)
- [Cells, spreadsheet-like expressiveness for CLOS](https://www.reddit.com/r/lisp/comments/7mji50/cells_spreadsheetlike_expressiveness_for_clos/)
- [cl-bibtex: A compatible re-implementation of the BibTeX program in Common Lisp, with a BST-to-CL compiler](https://github.com/mkoeppe/cl-bibtex)
- [C language syntax embedded in Common Lisp](https://github.com/y2q-actionman/with-c-syntax)
- [gendl - Generative Programming and Knowledge-based Engineering (KBE) system embedded in Common Lisp](https://gitlab.common-lisp.net/gendl/gendl)
- [Cognitive Robot Abstract Machine = Common Lisp + ROS](http://cram-system.org/doc/ide)
- [Esrap - a packrat parser for Common Lisp](https://scymtym.github.io/esrap/)
- [C-Mera, a Common Lisp source-to-source compiler to generate C/C++](https://www.reddit.com/r/lisp/comments/7oaum4/cmera_a_commonlisp_sourcetosource_compiler_to/)
- [cl-bench - Common Lisp benchmarking suite](https://www.reddit.com/r/Common_Lisp/comments/882mz4/clbench_common_lisp_benchmarking_suite/)
- [QGAME: Quantum and Gate Measurement Emulator](http://faculty.hampshire.edu/lspector/qgame.html)


# Articles

- [A Road to Common Lisp](http://stevelosh.com/blog/2018/08/a-road-to-common-lisp/) ([hacker news comments](https://news.ycombinator.com/item?id=17852194)). You should read this one.
- [How the strengths of Lisp-family languages facilitate building complex and flexible bioinformatics applications](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5952920/)
- [Writing a natural language date and time parser - internals of the Common Lisp library Chronicity](https://lisper.in/nlp-date-parser)
- [Implementing Hunchentoot custom sessions](https://www.darkchestnut.com/2018/hunchentoot_custom_sessions/)
- [Overview of Documentation Generators (codex, coo, declt, staple, cldomain)](https://lisp-journey.gitlab.io/blog/overview-of-documentation-generators/)
<!-- - [Challenging myself to learn Common Lisp in one month](https://github.com/TomLisankie/Learning-Lisp) -->
- [Converter of maps from Reflex Arena to QuakeWorld. cl-yacc, 3d-matrices](https://fourier.github.io/lisp/2019/01/02/reflex-map.html)
- [Debugging Common Lisp in Slime](https://two-wrongs.com/debugging-common-lisp-in-slime.html)
- [Packages in Common Lisp, a tutorial (pdf)](https://www-fourier.ujf-grenoble.fr/~sergerar/Papers/Packaging.pdf)
- [How to write test fixtures for FiveAM - Dark Chestnut](https://www.darkchestnut.com/2018/how-to-write-5am-test-fixtures/)
- [Franz and Semantic Web Co. Partner to Create a Noam Chomsky Knowledge Graph](https://allegrograph.com/franz-and-semantic-web-company-partner-to-create-a-noam-chomsky-knowledge-graph/)
- [Compiler basics: lisp to assembly](http://notes.eatonphil.com/compiler-basics-lisp-to-assembly.html)
- [Marvin Minsky - Scientist - The beauty of the Lisp language](https://www.webofstories.com/play/marvin.minsky/44)
- [Excavating a Common Treasure: Common Lisp](http://www.newresalhaider.com/post/common-treasure/)
- [Fun with Macros: If-Let and When-Let / Steve Losh](http://stevelosh.com/blog/2018/07/fun-with-macros-if-let/)
- [Extempore - The design, implementation and application of a cyber-physical programming language, Andrew Sorensen, Thesis, 2018 (PDF)](https://openresearch-repository.anu.edu.au/bitstream/1885/144603/1/Sorensen%20Thesis%202018.pdf)
- [Uniform Structured Syntax, Metaprogramming and Run-time Compilation](https://m00natic.github.io/lisp/manual-jit.html)
- [Simple expression evaluator comparison between Haskell, Rust, and Common Lisp](https://z0ltan.wordpress.com/2018/08/04/simple-expression-evaluator-comparison-between-haskell-rust-and-common-lisp/)
- [Lisping at JPL](https://www.reddit.com/r/lisp/comments/98s5zp/lisping_at_jpl/)

and also

- [Lisp, Jazz, Aikido: Three Expressions of a Single Essence](https://www.reddit.com/r/lisp/comments/8bshzw/lisp_jazz_aikido_three_expressions_of_a_single/)
- [Why lisp - biolisp](https://biolisp.github.io/#why-lisp)
- [Fun with Macros: Gathering / Steve Losh](http://stevelosh.com/blog/2018/05/fun-with-macros-gathering/)
- [Experience writing a full featured livejournal blog client in Common Lisp](http://can3p.github.io/blog/2018/06/19/client-writeup-intro/). Part 2: [client logic](http://can3p.github.io/blog/2018/06/22/client-writeup-logic/)
- [The (Un)common Lisp approach to Operations Research (2012)](https://www.reddit.com/r/lisp/comments/8o2c1y/the_uncommon_lisp_approach_to_operations_research/)
- [Alien: Return of Alien Technology to Classical Planning ](https://www.reddit.com/r/lisp/comments/8jq465/alien_return_of_alien_technology_to_classical/)
- [Emacs + ECL on Android](https://blog.teknik.io/phoe/p/1633)
- [Generic, consistent and dotted access of data structures with Access - lisp-journey](https://lisp-journey.gitlab.io/blog/generice-consistent-access-of-data-structures-dotted-path/) ([reddit](https://www.reddit.com/r/Common_Lisp/comments/7pysmx/generic_consistent_and_dotted_access_of_data/))
- [LLVM’s garbage collection facilities and SBCL’s generational GC](https://medium.com/@MartinCracauer/llvms-garbage-collection-facilities-and-sbcl-s-generational-gc-a13eedfb1b31)
- [A bunch of utilities from (again) sjl: higher order functions, sequences, debugging, profiling.](https://lisp-journey.gitlab.io/blog/snippets-functional-style-more/)
- [The return of cl-notebook](http://langnostic.inaimathi.ca/posts/the-return-of-cl-notebook)
- [Testing the SERIES package](https://www.reddit.com/r/lisp/comments/7rsfyn/testing_the_series_common_lisp_package/)

On games:

- [About Making Games in Lisp - Gamedev](https://reader.tymoon.eu/article/370)
- [Creating a (Non-Trivial) Lisp Game in 2018](https://defungames.com/2018/12/creating-a-non-trivial-lisp-game-in-2018/) (they just launched a [Crowdfunding](https://www.kickstarter.com/projects/defungames/spycursion-hacking-espionage-edutainment-mmo))
- [A Story of (defun games ())](https://old.reddit.com/r/Common_Lisp/comments/93g3p2/a_story_of_defun_games/)
- [Getting Started With trivial-gamekit](https://lthms.xyz/blog/lisp-journey-getting-started)

# Other screencasts

- [Lisp, The Quantum Programmer's Choice - Computerphile episode 2](https://www.youtube.com/watch?v=dw-y3vNDRWk)
- [McCLIM + Maxima: plot manipulation](https://www.youtube.com/watch?v=9VIT_Ml2v-Q)
- [McCLIM + Maxima: vector demo](https://www.youtube.com/watch?v=AvC82EjoPYU)
- [Comfy Lisp Programming - Project "Wikify" | Episode 2 @ 10am PST ](https://www.youtube.com/watch?v=CbfHpLUPL7E)
- [Common lisp and C++17 Live coding stream | TinyCDN CFFI Interop | Episode 13](https://www.reddit.com/r/lisp/comments/8ubnkn/common_lisp_and_c17_live_coding_stream_tinycdn/)
- [Growing a Lisp compiler - Amsterdam Lisp](https://www.youtube.com/watch?v=XT7JYPtWMd8)
- [Web Development in Emacs, Common Lisp and Clojurescript - Potato (Slack-like)](https://www.youtube.com/watch?v=bl8jQ2wRh6k)


# Discussion

- [Lisp and the remote agent - aka Lisp in a spacecraft - with an AMA of Ron Garret](https://www.reddit.com/r/lisp/comments/a7156w/lisp_and_the_remote_agent/)
- [How to make (Common) Lisp popular?](https://www.reddit.com/r/lisp/comments/a5ggd4/how_to_make_common_lisp_popular/)
- [Feedback from a new LispWorks user](https://www.reddit.com/r/Common_Lisp/comments/9r9xy6/feedback_from_a_new_lispworks_user/) ([how is LispWorks the company going ?](https://www.reddit.com/r/lisp/comments/9qh3op/how_is_lispworks_the_company_doing/))
- [How do you normally use a program once written ?](https://www.reddit.com/r/Common_Lisp/comments/a3r4hb/how_do_you_normally_use_a_program_once_written/)
- [Structs vs Parametric Polymorphism (an answer to the "switching from Common Lisp to Julia - thoughts ?" post)](https://www.reddit.com/r/lisp/comments/a10629/structs_vs_parametric_polymorphism_an_answer_to/) also [this discussion](https://www.reddit.com/r/lisp/comments/9y425b/switching_from_common_lisp_to_julia_your_thoughts/)
- [How to work on a project and make sure dependencies are tracked correctly?](https://www.reddit.com/r/Common_Lisp/comments/9os171/how_to_work_on_a_project_and_make_sure/)
- [Does anyone else hate LOOP?](https://www.reddit.com/r/lisp/comments/a2hkq0/does_anyone_else_hate_loop_cl/)
- [What does it take to understand the true power of Lisp?](https://www.reddit.com/r/lisp/comments/9xd4gy/what_does_it_take_to_understand_the_true_power_of/)
- [How did Lisp make your life easier ?](https://www.reddit.com/r/lisp/comments/9qfrxe/how_lisp_made_your_life_easier/)
- [Should local variables be avoided when possible when doing functional programming?](https://www.reddit.com/r/lisp/comments/9zpts4/should_local_variables_be_avoided_when_possible/)
- [Is ABCL an active project and does it support JRE 1.11?](https://www.reddit.com/r/lisp/comments/9upt86/abcl/)
- [Has the Gnu Coreutils ever been implemented in Lisp? If not, would that be a worthwhile project?](https://www.reddit.com/r/lisp/comments/9q68y8/has_the_gnu_coreutils_ever_been_implemented_in/)
- [Common Lisp and Machine Learning these days](https://www.reddit.com/r/Common_Lisp/comments/8bn7f9/common_lisp_and_machine_learning_these_days/)
- [Has anyone considered or started a project to write a CL implementation in WebAssembly?](https://www.reddit.com/r/lisp/comments/7z7wuq/has_anyone_considered_or_started_a_project_to/)
- [What do you recommend to work with SQL databases ? What's your experience with Mito ?](https://www.reddit.com/r/Common_Lisp/comments/7s53qi/what_do_you_recommend_to_work_with_sql_databases/) and [sqlite only interface: cl-sqlite or cl-dbi ?](https://www.reddit.com/r/Common_Lisp/comments/7ozdcf/sqliteonly_interface_clsqlite_or_cldbi/)
  and [is there an ORM that generates classes from table definitions ?](https://www.reddit.com/r/lisp/comments/7iebty/ask_rlisp_is_there_any_lightweight_orm_that/)

## Learning Lisp

- [I want to try Lisp, how should I begin?](https://www.reddit.com/r/lisp/comments/9er4mo/i_want_to_try_lisp_how_should_i_begin/)
- [What lisp dialect for "real world" applications?](https://www.reddit.com/r/lisp/comments/9b7v56/what_lisp_dialect_for_real_world_applications/)
- [What do commercial Lisps offer that frees don't?](https://www.reddit.com/r/lisp/comments/8w8cr1/what_do_commercial_lisps_offer_that_frees_dont/)
- [Which (non-Clojure) Lisp to learn first?](https://www.reddit.com/r/lisp/comments/9kiji7/which_nonclojure_lisp_to_learn_first/)
- [Can CL implement Clojure's keyword as function syntax?](https://www.reddit.com/r/lisp/comments/9k2vmi/can_cl_implement_clojures_keyword_as_function/)
- [Why did you decide to learn Lisp?](https://www.reddit.com/r/lisp/comments/8jzpzw/why_did_you_decide_to_learn_lisp/)
- [How do you go about starting a Common Lisp Project? A beginner looking for pointers.](https://www.reddit.com/r/lisp/comments/8j9ing/how_do_you_go_about_starting_a_common_lisp/)
- [As a newbie, what I will miss if I choose Racket over Common Lisp? Or if I happen to learn both at somepoint in future, choosing Racket/Common Lisp now would make sense?](https://www.reddit.com/r/lisp/comments/86ze0t/as_a_newbie_what_i_will_miss_if_i_choose_racket/)
- [What can other languages do that Lisp can't ?](https://www.reddit.com/r/lisp/comments/7lf149/what_can_other_languages_do_that_lisp_cant/)


## Common Lisp VS ...

<!-- not sure if listing some of those is constructive, really; and the Clojure vs Common Lisp wars are getting really tiring...  and this coming from an active flamer (defunkydrummer) -->

- [How did the Common Lisp community survived without the equivalent of clojure.spec ?](https://www.reddit.com/r/Common_Lisp/comments/ac9mm9/how_common_lisp_community_survived_without_the/)
- [Is there a Lisp that is considered "excellent" about error handling ?](https://www.reddit.com/r/lisp/comments/8wdw2r/is_there_a_lisp_that_is_considered_excellent/)
- [Lisp Dialect survey](https://www.reddit.com/r/lisp/comments/9lhbnx/lisp_dialect_survey/)
- [the Julia challenge](https://www.reddit.com/r/lisp/comments/9i5mmp/the_julia_challenge/)
- [Python pitfalls ?](https://www.reddit.com/r/lisp/comments/9g07to/python_pitfalls/)
- [How a Common Lisp programmer views users of other languages (humor)](https://www.reddit.com/r/lisp/comments/9lpssp/how_a_common_lisp_programmer_views_users_of_other/)
- [Miller School Researchers Help Push the Limits of Programming Languages in Biology](http://med.miami.edu/news/miller-school-researchers-help-push-the-limits-of-programming-languages-in-)
- [Lisp vs Java (thought you guys might find this humorous)](https://www.reddit.com/r/lisp/comments/8mcts3/lisp_vs_java_thought_you_guys_might_find_this/)
- [What other languages besides Lisp do you enjoy programming in?](https://www.reddit.com/r/lisp/comments/8q6gaa/what_other_languages_besides_lisp_do_you_enjoy/)

---

Enjoy the material, and see you soon !
