+++
date = "2017-10-19T07:51:49+01:00"
title = "Lisp software"
draft = false
+++

There's a fantastic showcase of succesfull Common Lisp software on
lisp-lang.org: http://lisp-lang.org/success/ so go there first.

However all are not open source and it doesn't list new or not so
awesome but very cool or interesting software. That's what we'll do
here to see that yeah Lisp is used today and to have code bases to
look at.

If you are looking for CL *libraries*, you of course had a look to the
[Awesome CL](https://github.com/CodyReichert/awesome-cl) list.

## Awesome Lisp software

- [pgloader](https://github.com/dimitri/pgloader) - **re-written from
  Python with a 20 to 30x speed gain** and more O_o -
  [blog post](http://tapoueh.org/blog/2014/05/14-pgloader-got-faster.html)
  By Dimitri Fontaine working at Postgres in 2014. He also maintains
  around 50 Debian packages of Lisp libraries for these needs. He gave
  a
  [lightning talk at the 7th European Lisp Symposium](http://tapoueh.org/confs/2014/05/05-ELS-2014). Uses [Postmodern](http://marijnhaverbeke.nl/postmodern/) and [lparallel](http://lparallel.org/) for asynchronous IO. Also,

> the new code base and feature set seems to attract way more users than the previous implementation ever did, despite using a less popular programming language.

- [pgchart](https://github.com/dimitri/pgcharts) - a **self-contained web application** that takes as input an SQL query text and outputs its data as a chart. By the same Dimitri Fontaine.

- [potato](https://github.com/cicakhq/potato) - a **Slack-like conversation platform**. Many features. CL in the backend, ClojureScript to the frontend. Apache Solr, RabbitMQ, email updates,… Web, Emacs and Android clients. [web coding video](https://www.youtube.com/watch?v=bl8jQ2wRh6k).

- [turtl](https://github.com/turtl/api) - a security focused online **note taking app**. Deployed at scale at [Framanotes](https://framanotes.org/). Backend in CL.

### Internet

- [nEXT browser](https://github.com/nEXT-Browser/nEXT) - Qt based **web browser**.

### Publishing software

- [Coleslaw](https://github.com/kingcons/coleslaw/) - a **static site generator** similar to Jekyll. With nifty features (build on git push,…). Example blog: http://40ants.com/
- [Radiance](https://shirakumo.github.io/radiance/) - publishing software, between CMS and framework.
- [Reader](https://github.com/Shirakumo/reader) - a simple blogging platform for Radiance.
- [Purplish](https://github.com/Shirakumo/purplish) - an imageboard app for Radiance.

### Editors

- [Darkmatter](https://github.com/tamamu/darkmatter) - Common Lisp **notebook** (also exists cl-jupyter). Built on Clack.

- [Lem](https://github.com/cxxxr/lem) - an Emacs clone tailored for Common Lisp development, for the terminal or Electron. [Screencast](https://www.youtube.com/watch?v=YkSJ3p7Z9H0).


### GUI apps

- [Halftone](https://github.com/Shinmera/halftone) - a multiplatform
  and portable image viewer. Simple app to demo building Qt GUIs with
  Qtools.

### Terminal apps

`todo: enhance !`

- [Pml](https://github.com/pierre-lecocq/pml/blob/master/src/parse.lisp) - cli tool to parse my nginx logs and print statistics (and [Tankfeeder for apache](https://bitbucket.org/mihailp/tankfeeder/src/ccb6025348243bb98cb3ec27810501492313861f/apache/?at=default))
- [shuffletron](http://vintage-digital.com/hefner/software/shuffletron/) -
  a terminal music player. [staling]
- [shtookovina](https://github.com/mrkkrp/shtookovina) - a program to
  help learn natural languages, based on audio recording by the
  [Shtooka project](http://shtooka.net/), with an advanced readline
  interface and fully hackable in Common Lisp. Deprecated since 2015.
