---
title: "cl-torrents, app and (extensive) tutorial: web scraping and building executables"
date: 2017-12-20T14:21:20+01:00
draft: false
---

Lately we exercised our Lisp skills by writing
[cl-torrents](https://github.com/vindarel/cl-torrents), an app that
**searches for torrents** on several sources (the Pirate Bay through
piratebay.to, Kickass torrents and torrent.cd), and we wrote an
[extensive tutorial](https://vindarel.github.io/cl-torrents/tutorial.html)
in the making (that was actually our primary goal). It comes as a
library to use from the REPL and as a **self-contained executable**
(download and run, nothing more to install). You'll find the following
topics in the tutorial:

* how to **create and load a new project**,
* common pitfalls, basic data structures, useful libraries, where to find documentation,
* (async) **web scraping**,
* **unit testing, with mocks**,
* **continuous integration** and delivery of executables (Gitlab CI, Docker),
* parsing **command line arguments**,
* **building self-contained executables**,
* basics of **error handling**,
* …

Some topics have been ported to the Cookbook, some not (yet).

![](https://vindarel.github.io/cl-torrents/img-colored-results.png)

The next iteration will be about a self-contained web app.

![](https://raw.githubusercontent.com/vindarel/cl-torrents/master/img-cli.png)
