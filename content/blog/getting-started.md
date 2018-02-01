---
title: "Getting started: how to install a Common Lisp development environment"
date: 2017-05-03T10:41:33+02:00
draft: false
---

or "could not find recent and easy installation steps [fixed]".

When I started I was a bit confused by old instructions (google is not
good at CL), so hopefully this post will help show up recent and easy
steps and most of all, help every CL enthousiast discover
**Portacle**.

(and this post is editable through its [Gitlab repository](https://gitlab.com/lisp-journey/lisp-journey.gitlab.io/))

## Portable, a multiplatform development environment

The productive Shinmera was waiting for the last details to be fixed
before showing [Portacle](https://portacle.github.io/) but it was
already great. On GNU/Linux, MacOs or Windows, just download an
archive and click an icon to open Emacs ready to use for CL
development. It is that easy.

It ships: Emacs (customized), the SBCL implementation, Slime (Emacs
IDE), Quicklisp (package manager) and Git. Emacs comes with a nice
theme, autocompletion in drop-downs (company-mode) and
[Magit](https://magit.vc/).

## Manual install

### Lisp implementation

Install a CL implementation:

    apt-get install sbcl

Now you can run `sbcl` and write lisp at the prompt:

~~~lisp
(print "hello lisp!")
(quit) ;; or C-d
~~~

More are packaged for Debian and probably for your distro, notably
[ECL](https://gitlab.com/embeddable-common-lisp/ecl/), and note that
you can install more easily with [Roswell](https://github.com/roswell/roswell/wiki).

If you find the prompt horribly unfriendly (no history, no navigation…) use rlwrap:

    apt-get install rlwrap

and now this will be slightly better:

    rwrap sbcl

Even better, a slight wrapper around the SBCL REPL with readline
support (Emacs and Vim modes, history, etc):
[sbcli](https://github.com/hellerve/sbcli), straightforward to use.

But still, we really need an editor.


### Editors support

You're not bound to Emacs, there's good support for Vim, Sublime Text
(via the SublimeREPL package) and Atom.

See the [Cookbook#editors](https://lispcookbook.github.io/cl-cookbook/editor-support.html).

For Emacs, [Slime](https://github.com/slime/slime) is the de-facto
solution (there's also the Sly fork). It is in the GNU Elpa default
Emacs package repository, so:

    M-x package-install RET slime RET

(you may need a `M-x package-refresh-content`).

Now start Slime with `M-x slime` and wait a few seconds that it starts
its backend (Swank server).


Might help:

- using Emacs (and other instructions): https://www.darkchestnut.com/2017/getting-started-with-common-lisp/#using-emacs
- http://wikemacs.org/wiki/Common_Lisp
- Slime manual: https://common-lisp.net/project/slime/doc/html/ (see
  the Emacs menu). In very short: compile a file with `C-c C-k`,
  compile one function with `C-c C-c` and use it at the REPL.

### Quicklisp package manager

To install [Quicklisp](https://www.quicklisp.org/beta/):

from anywhere, download this file:

     wget https://beta.quicklisp.org/quicklisp.lisp

start a Lisp and load this file:

    sbcl --load quicklisp.lisp

we get in the sbcl prompt. We have one Quicklisp command to type to
install it:

    (quicklisp-quickstart:install)

it will install itself in `~/quicklisp/quicklisp/`.

it should output something like this, showing the basic commands:

```
==================================================
2,846 bytes in 0.001 seconds (2779.30KB/sec)
Upgrading ASDF package from version 2.004 to version 2.009
; Fetching #<URL "http://beta.quicklisp.org/dist/quicklisp.txt">
; 0.40KB
==================================================
408 bytes in 0.003 seconds (132.81KB/sec)

  ==== quicklisp installed ====

    To load a system, use: (ql:quickload "system-name")

    To find systems, use: (ql:system-apropos "term")

    To load Quicklisp every time you start Lisp, use: (ql:add-to-init-file)

    For more information, see http://www.quicklisp.org/beta/

NIL
```

Does it work ? Let's try to install something:

    (ql:quickload "dexador")

It is installed but we want to have Quicklisp available everytime we
start sbcl. Otherwise we'd have to `load` the file located at
`~/quicklisp/quicklisp/setup.lisp`.

Each implementation uses a startup file, like our shells, so we can
add this into our `~/.sbclrc`:

~~~lisp
;;; The following lines added by ql:add-to-init-file:
  #-quicklisp
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init)))
~~~

To quit sbcl, `(quit)` or `C-d`.

Quicklisp is a bit different than others package managers and it is
not the only solution. That's for another post.

## Starting a project

I advise [cl-project](https://github.com/fukamachi/cl-project) which,
unlike others (quickproject) also sets up tests.

Now we can `C-c C-k` the `.asd` file and `(ql:quickload "my-app")` our
app in the Slime REPL. But this is for another post.

## Managing implementations and installing libraries in the command line: Roswell

This is done together with [Roswell](https://github.com/roswell/roswell/wiki).

Roswell is in brew for MacOS, in linuxbrew, and it has a Debian package.

It allows to install pinned versions of SBCL or of other
implementations (Embedable CL, Clozure CL,…) easily:

    ros install sbcl/1.2.14
    ros install sbcl  # the latest
    ros install ccl-bin

what's available ?

    ros list versions

change the current lisp:

    ros use sbcl/1.2.14

Install scripts:

    ros install qlot

Install packages:

    ros install dexador  # http client


and it does more to help scripting and distributing software. See its wiki !

## See also

- a Debian package for CCL (2016): http://mr.gy/blog/clozure-cl-deb.html
