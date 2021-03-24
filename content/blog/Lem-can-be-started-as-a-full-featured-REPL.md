---
title: "Lem can now be started as a full featured Lisp REPL"
date: 2021-03-24T12:43:13+01:00
draft: false
---

The [Lem editor](https://github.com/lem-project/lem/), which supports
Common Lisp as well as other languages, works by default in the
terminal with a ncurses frontend (it also has an experimental Electron
frontend). It ships a nice Lisp REPL: it has good fuzzy completion,
enough keyboard shortcuts, an interactive debugger, a completion menu,
etc.

It is now possible to run Lem straight in its Lisp REPL. Run it with:

    lem --eval "(lem-lisp-mode:start-lisp-repl t)"

The optional argument (`t`) was added recently (thanks, @cxxxr) and allows to start the REPL in fullscreen.

Here is it in action:

![](/lem1.png)

![](/lem2.png)

![](/lem3.png)

![](/lem4.png)

The other terminal-based REPL alternatives are:

- [cl-repl](https://github.com/koji-kojiro/cl-repl/) (wants to be full-featured, à la ipython)
- [sbcli](https://github.com/hellerve/sbcli) (handy, but minimalistic)

but IMO, they now fall short compared to Lem's features.


## Installation

To install it, see its [wiki](https://github.com/lem-project/lem/wiki). In short, do

     ros install cxxxr/lem

or use [lem-docker](https://github.com/40ants/lem-docker).

To install Roswell, do one of

    brew install roswell  # macos or linuxbrew
    yay -S roswell
    scoop install roswell  # windows

    # Debian package:
    curl -sOL `curl -s https://api.github.com/repos/roswell/roswell/releases/latest | jq -r '.assets | .[] | select(.name|test("deb$")) | .browser_download_url'`
    sudo dpkg -i roswell.deb

See its [releases](https://github.com/roswell/roswell/releases).


## Usage (quickref)

Lem has Emacs-like keybindings, as well as a vi emulation (`M-x vi-mode`). Unfortunately, that is not documented much.

So, to open a file, press `C-x C-f` (you get the file selection dialog shown above). To save it, it's `C-x C-s`.

To switch to the REPL: `C-c C-z`.

To compile and load a buffer: `C-c C-k`. To compile a function: `C-c C-c`.

To switch windows: `C-x o`. To make a window fullscreen: `C-x 1`. To split it vertically: `C-x 3` and horizontally: `C-x 2`.

To switch buffers: `C-x b`.

To run an interactive command: `M-x` (`alt-x`).

See this Emacs & Slime cheatsheet to find more: https://lispcookbook.github.io/cl-cookbook/emacs-ide.html#appendix

Lem works out of the box for several programming languages (Python, Rust, Scala…). It also has an HTML mode, a directory mode… Lem needs to be discovered!


- chat: https://gitter.im/lem-developers/community
