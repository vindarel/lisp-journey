---
title: "These Months in Common Lisp Q4 2018"
date: 2019-01-15T20:01:30+02:00
draft: false
---

- [Q1 2018](https://lisp-journey.gitlab.io/blog/these-months-in-common-lisp-q1-2018/)
- [Q2 2018](https://lisp-journey.gitlab.io/blog/these-months-in-common-lisp-q2-2018/)
- [Q3 2018](https://lisp-journey.gitlab.io/blog/these-months-in-common-lisp-q3-2018/)

I wanted to do this for a year and here we are ! I don't think I'll
carry on, with this format at least.

If I missed anything crucial: you have comments and PRs: https://gitlab.com/lisp-journey/lisp-journey.gitlab.io/

Happy (re)discoveries !


# Documentation

- [Debugging – the Common Lisp Cookbook](https://lispcookbook.github.io/cl-cookbook/debugging.html)
- [Loop, iteration, mapping – the Common Lisp Cookbook](https://lispcookbook.github.io/cl-cookbook/iteration.html)
- [cl-exercise: Common Lisp Learning System running on browsers](https://www.reddit.com/r/lisp/comments/a9at82/clexercise_common_lisp_learning_system_running_on/)

# Announcements

- various [SBCL releases](http://www.sbcl.org/all-news.html) (from 1.4.13 to 1.4.15) ([not many changes you think ?](https://www.reddit.com/r/lisp/comments/9s6nen/sbcl_1413_released/))
- [Release of Corman Lisp 3.1 (Windows)](https://www.reddit.com/r/Common_Lisp/comments/aaxp2c/release_corman_lisp_31_sharplisperscormanlisp/), with [personal notes](https://chaoticlab.io/lisp/update/2018/12/30/corman-3-1-release.html) (also [this thread](https://www.reddit.com/r/Common_Lisp/comments/a3hhpa/the_upcoming_release_of_corman_lisp_31_issue_40/))
- [European Lisp Symposium 2019 - Call for Papers](https://european-lisp-symposium.org/2019/index.html)
- [Dandelion, Eclipse IDE plugin, updated for SBCL 1.4.10](https://github.com/Ragnaroek/dandelion)
- [December Quicklisp update](http://blog.quicklisp.org/2018/12/december-2018-quicklisp-dist-update-now.html), [october](http://blog.quicklisp.org/2018/10/october-2018-quicklisp-dist-update-now.html)
- [Common Lisp is now available to use at Kattis](https://open.kattis.com/help)

# Projects

- [CANDO - A Computational Chemistry programming environment integrating Common Lisp and C++ based on the Jupyter notebook](https://hub.docker.com/r/drmeister/cando/)
- [Coalton, a dialect of ML embedded in Common Lisp (alpha)](https://github.com/tarballs-are-good/coalton)
- [Ulubis - A Wayland compositor written in Common Lisp](Ulubis - A Wayland compositor written in Common Lisp)
- [ISO 8601 date/time library](https://gitlab.com/DataLinkDroid/iso-8601-date)
- [Voxel game engine (Minecraft)](https://github.com/terminal625/sucle)
- [magrathea: chaotic-neutral web security for Hunchentoot](https://gitlab.com/Theemacsshibe/magrathea)
- [schannel: Common Lisp Windows SChannel API](https://github.com/fjames86/schannel)
- [qbase64: A fast and flexible base64 encoder/decoder in Lisp](https://www.reddit.com/r/Common_Lisp/comments/9qguq3/qbase64_a_fast_and_flexible_base64_encoderdecoder/)
- [Beautify Practical Common Lisp, Firefox extension](https://addons.mozilla.org/en-US/firefox/addon/beautify-practical-common-lisp/)
- [static-dispatch: Static generic function dispatch. The purpose is to provide an optimization in cases where the usual dynamic dispatch is too slow, and the dynamic features are not required](https://github.com/alex-gutev/static-dispatch/)
- [cl-http2-protocol: HTTP/2 interop library in Common Lisp](https://github.com/akamai/cl-http2-protocol)
- [cl-punch: Scala-like and CL21-like anonymous lambda literal](cl-punch: Scala-like and CL21-like anonymous lambda literal) See other [lambda shorthands](https://github.com/CodyReichert/awesome-cl#lambda-shorthands).
- [Scheme macros for Common Lisp](http://www.ccs.neu.edu/home/dorai/mbe/mbe-lsp.html)
- [The INVAL plan validator, and other PDDL tools](https://github.com/patrikhaslum/INVAL)
- [Australian Government statistics collection library](https://gitlab.com/DataLinkDroid/slk-581)
- [Easy local bindings](https://www.reddit.com/r/Common_Lisp/comments/aaxsas/easybind_easy_local_binding_for_common_lisp/)
- [cl-fm - a file manager using cl-cffi-gtk (seems staling. "not ready for prime time")](https://gitlab.com/stacksmith/cl-fm)
- [cl-intervals: Intervals and interval trees for Common Lisp](https://github.com/rpav/cl-interval)

GUI:

- [nodgui - yet another Tcl/Tk-based GUI package for Common Lisp](https://notabug.org/cage/nodgui)
- [YstokWidgets Professional Edition](http://en.ystok.ru/products/ywidgets/)
- [MIDGETS - A collection of CAPI widgets and utilities](https://common-lisp.net/~loliveira/ediware/midgets/doc/)
- [subtext: A mostly-text-based UI bridges Common Lisp objects and runs of text. Minimal text-based user interface](https://github.com/stacksmith/subtext)

Developer utilities:

- [s2i-lisp: Common Lisp + Quicklisp OpenShift Build Image](https://github.com/hjudt/s2i-lisp)
- [lisp-images: Docker images for common lisp development](https://github.com/fisxoj/lisp-images) (with some others, see the awesome-list)
- [Quicklisp.nvim - Common Lisp package management within Neovim](https://gitlab.com/HiPhish/quicklisp.nvim)


New releases:

- [JSCL 0.7.0 now supports CLOS thanks to the work of vlad-km](https://jscl-project.github.io/)
- [Next browser 1.2.0 is out!](https://www.reddit.com/r/lisp/comments/a954yf/next_browser_120_is_out/)
- [Lem editor 1.5 released with executables, rust-mode, nim-mode, html-mode, jsx, calc-mode, ncurses for windows, experimental lsp-mode, support for async processes, python and scheme repl and more](https://github.com/cxxxr/lem/releases)

(re)discoveries:

- [cl-rest-server: Serve REST APIs from Common Lisp, Swagger support ](https://github.com/mmontone/cl-rest-server)
- [lfarm - a library for distributing work across machines (on top of lparallel and usocket)](https://github.com/lmj/lfarm)
- [cl-docutils: implementation of Docutils. Includes a parser for the reStructured format, writers to html and latex](https://github.com/willijar/cl-docutils)
- [formulador: render math formulas in 2D in your terminal!](https://github.com/tarballs-are-good/formulador)
- [cl-bibtex: A compatible re-implementation of the BibTeX program in Common Lisp, with a BST-to-CL compiler](https://github.com/mkoeppe/cl-bibtex)
- [clcon - a Common Lisp editor (tcl/tk, mostly russian)](https://bitbucket.org/budden/clcon/wiki/Screenshots)
- [C language syntax embedded in Common Lisp](https://github.com/y2q-actionman/with-c-syntax)



# Articles

- [This Old Lisp (on CCL)](https://www.reddit.com/r/Common_Lisp/comments/9pf2nd/this_old_lisp/)
- [How the strengths of Lisp-family languages facilitate building complex and flexible bioinformatics applications](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5952920/)
- [Writing a natural language date and time parser - internals of the Common Lisp library Chronicity](https://lisper.in/nlp-date-parser)
- [validate-superclass explained](https://nl.movim.eu/?blog/phoe%40movim.eu/a9391f4b-485e-4f3a-ae02-051a5fc65ed1)
- [CFFI arrays versus STATIC-VECTORS: a comparison](https://nl.movim.eu/?blog/phoe%40movim.eu/cffi-arrays-versus-static-vectors-a-comparison-SCutJQ)
- [Dumping Common Lisp streams](https://nl.movim.eu/?blog/phoe%40movim.eu/82419fb5-a24c-4350-a9c8-c7ae8c6aa2a6)
- [Killing Common Lisp methods and classes](https://nl.movim.eu/?blog/phoe%40movim.eu/killing-common-lisp-methods-and-classes-b23ADB)
- [Funny method combinations](http://eshamster.hatenablog.com/entry/play-define-method-combination-01)
- [Implementing Hunchentoot custom sessions](https://www.darkchestnut.com/2018/hunchentoot_custom_sessions/)
- [Overview of Documentation Generators (codex, coo, declt, staple, cldomain)](https://lisp-journey.gitlab.io/blog/overview-of-documentation-generators/)
- [Challenging myself to learn Common Lisp in one month](https://github.com/TomLisankie/Learning-Lisp)
- [Converter of maps from Reflex Arena to QuakeWorld. cl-yacc, 3d-matrices](https://fourier.github.io/lisp/2019/01/02/reflex-map.html)
- [Debugging Common Lisp in Slime](https://two-wrongs.com/debugging-common-lisp-in-slime.html)
- [Packages in Common Lisp, a tutorial (pdf)](https://www-fourier.ujf-grenoble.fr/~sergerar/Papers/Packaging.pdf)
- [How to write test fixtures for FiveAM - Dark Chestnut](https://www.darkchestnut.com/2018/how-to-write-5am-test-fixtures/)
- [Franz and Semantic Web Co. Partner to Create a Noam Chomsky Knowledge Graph](https://allegrograph.com/franz-and-semantic-web-company-partner-to-create-a-noam-chomsky-knowledge-graph/)
- [Composing in Lisp with Csound](https://michaelgogins.tumblr.com/post/178126207468/composing-in-lisp) (see also [audio and music composition software](https://github.com/CodyReichert/awesome-cl#audio))
- [Blogging with Lisp](https://terranostra.one/posts/Blogging-with-Lisp.html)
- [Compiler basics: lisp to assembly](http://notes.eatonphil.com/compiler-basics-lisp-to-assembly.html)
- [Marvin Minsky - Scientist - The beauty of the Lisp language](https://www.webofstories.com/play/marvin.minsky/44)

GUIs:

- [McCLIM Yule progress report - towards 1.0](https://common-lisp.net/project/mcclim/posts/Yule-progress-report.html)
- [Demo SBCL script using Gtk](https://www.reddit.com/r/lisp/comments/a31oxr/demo_sbcl_script_using_gtk/)

On games:

- [Baggers responds to 'Reasons why Lisp games suffer'](http://techsnuffle.com/2018/12/07/reasons-why-lisp-games-suffer-corrections)
- [About Making Games in Lisp - Gamedev](https://reader.tymoon.eu/article/370)
- [Creating a (Non-Trivial) Lisp Game in 2018](Creating a (Non-Trivial) Lisp Game in 2018)


# Discussion

- [Lisp and the remote agent - with an AMA of Ron Garret](https://www.reddit.com/r/lisp/comments/a7156w/lisp_and_the_remote_agent/)
- [How to make (Common) Lisp popular?](https://www.reddit.com/r/lisp/comments/a5ggd4/how_to_make_common_lisp_popular/)
- [Feedback from a new LispWorks user](https://www.reddit.com/r/Common_Lisp/comments/9r9xy6/feedback_from_a_new_lispworks_user/) ([how is LispWorks the company going ?](https://www.reddit.com/r/lisp/comments/9qh3op/how_is_lispworks_the_company_doing/))
- [How do you normally use a program once written ?](https://www.reddit.com/r/Common_Lisp/comments/a3r4hb/how_do_you_normally_use_a_program_once_written/)
- [How does Common Lisp implement hot code reloading?](https://www.reddit.com/r/Common_Lisp/comments/9q6bum/how_does_common_lisp_implement_hot_code_reloading/)
- [Structs vs Parametric Polymorphism (an answer to the "switching from Common Lisp to Julia - thoughts ?" post)](https://www.reddit.com/r/lisp/comments/a10629/structs_vs_parametric_polymorphism_an_answer_to/) also [this discussion](https://www.reddit.com/r/lisp/comments/9y425b/switching_from_common_lisp_to_julia_your_thoughts/)
- [How to work on a project and make sure dependencies are tracked correctly?](https://www.reddit.com/r/Common_Lisp/comments/9os171/how_to_work_on_a_project_and_make_sure/)
- [Does anyone else hate LOOP?](https://www.reddit.com/r/lisp/comments/a2hkq0/does_anyone_else_hate_loop_cl/)
- [What does it take to understand the true power of Lisp?](https://www.reddit.com/r/lisp/comments/9xd4gy/what_does_it_take_to_understand_the_true_power_of/)
- [How did Lisp make your life easier ?](https://www.reddit.com/r/lisp/comments/9qfrxe/how_lisp_made_your_life_easier/)
- [Should local variables be avoided when possible when doing functional programming?](https://www.reddit.com/r/lisp/comments/9zpts4/should_local_variables_be_avoided_when_possible/)
- [Is ABCL an active project and does it support JRE 1.11?](https://www.reddit.com/r/lisp/comments/9upt86/abcl/)
- [Has the Gnu Coreutils ever been implemented in Lisp? If not, would that be a worthwhile project?](https://www.reddit.com/r/lisp/comments/9q68y8/has_the_gnu_coreutils_ever_been_implemented_in/)
- ["Classes are not interactions!" by shka](https://sirherrbatka.github.io/blog/2018/10/14/classes-are-not-interactions/)

# Screencasts

- [2018 LLVM Developers’ Meeting: C. Schafmeister “Lessons Learned Implementing Common Lisp with LLVM”](https://www.youtube.com/watch?v=mbdXeRBbgDM&app=desktop)
- [A pile of parens](https://www.youtube.com/channel/UCMV8p6Lb-bd6UZtTc_QD4zA)
- [Pushing Pixels with Lisp - 61 - Stenciling (Failed attempt)](https://youtu.be/pZO6HSKHWLg)

# Common Lisp VS ...

- [How did the Common Lisp community survived without the equivalent of clojure.spec ?](https://www.reddit.com/r/Common_Lisp/comments/ac9mm9/how_common_lisp_community_survived_without_the/)
