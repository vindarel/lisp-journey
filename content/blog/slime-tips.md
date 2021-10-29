---
title: "Slime Tips"
date: 2019-08-26T10:40:46+02:00
tags: ["tutorial", "tip",]
draft: false
---

Recently on reddit there was a reminder about
[lisptips.com](https://lisptips.com/) and
[slime-tips](https://slime-tips.tumblr.com/). I already knew the two,
but this time I fully enjoyed the Slime tips. I copy my favourites.

As usual, I enhanced the [Cookbook/emacs-ide.html](https://lispcookbook.github.io/cl-cookbook/emacs-ide.html) at the same time.

The Slime documentation is here: https://common-lisp.net/project/slime/doc/html/

## Documentation lookup

- **C-c C-d h**  looks up documentation in CLHS. But it works only on symbols, so there are two more bindings:
- **C-c C-d #** for reader macros
- **C-c C-d ~**  for format directives

Other bindings which may be useful:

- **C-c C-d d**  describes a symbol using `describe`
- **C-c C-d f**  describes a function using `describe`

## Synchronizing packages

**C-c ~** (*slime-sync-package-and-default-directory*): When run in a
buffer with a lisp file it will change the current package of the REPL
to the package of that file and also set the current directory of the REPL
to the parent directory of the file.

## Calling code

**C-c C-y** (*slime-call-defun*): When the point is inside a defun and
C-c C-y is pressed,

(I’ll use [] as an indication where the cursor is)


~~~lisp
(defun foo ()
 nil[])
~~~


then `(foo [])` will be inserted into the REPL, so that you can write
additional arguments and run it.


If `foo` was in a different package than the package of the REPL,
`(package:foo )` or `(package::foo )` will be inserted.

This feature is very useful for testing a function you just wrote.

That works not only for defun, but also for defgeneric, defmethod,
defmacro, and define-compiler-macro in the same fashion as for defun.

For defvar, defparameter, defconstant: `[] *foo*` will be inserted
(the cursor is positioned before the symbol so that you can easily
wrap it into a function call).

For defclass: `(make-instance ‘class-name )`.

**Inserting calls to frames in the debugger**

**C-y** in SLDB on a frame will insert a call to that frame into the REPL, e.g.,

```
(/ 0) =>
…
1: (CCL::INTEGER-/-INTEGER 1 0)
…
```

**C-y** will insert `(CCL::INTEGER-/-INTEGER 1 0)`.

(thanks to [Slime tips](https://slime-tips.tumblr.com/page/2))

## Exporting symbols

**C-c x** (*slime-export-symbol-at-point*) from the `slime-package-fu`
contrib: takes the symbol at point and modifies the `:export` clause of
the corresponding defpackage form. It also exports the symbol.  When
called with a negative argument (C-u C-c x) it will remove the symbol
from `:export` and unexport it.

**M-x slime-export-class** does the same but with symbols defined
by a structure or a class, like accesors, constructors, and so on.
It works on structures only on SBCL and Clozure CL so far.
Classes should work everywhere with MOP.

Customization

There are different styles of how symbols are presented in
`defpackage`, the default is to use uninterned symbols (`#:foo`).
This can be changed:

to use keywords:


~~~lisp
(setq slime-export-symbol-representation-function
      (lambda (n) (format ":%s" n)))
~~~

or strings:

~~~lisp
(setq slime-export-symbol-representation-function
 (lambda (n) (format "\"%s\"" (upcase n))))
~~~

### Crossreferencing: find who's calling, referencing, setting a symbol

Slime has a nice cross referencing facility, for example, you can see
what calls a particular function or expands a macro.  It presents a
list of places which reference a particular entity, from there you can
recompile the thing which references by pressing **C-c C-c** on that
line. **C-c C-k** will recompile all the references. This is useful when
modifying macros, inline functions, or constants.

The following bindings are also shown in Slime's menu:

- **C-c C-w c** *slime-who-calls* callers of a function
- **C-c C-w m** *slime-who-macroexpands* places where a macro is expanded
- **C-c C-w r** *slime-who-references* global variable references
- **C-c C-w b** *slime-who-bind* global variable bindings
- **C-c C-w s** *slime-who-sets* global variable setters
- **C-c C-w a** *slime-who-specializes* methods specialized on a symbol

And when the `slime-asdf` contrib is enabled,
**C-c C-w d** *slime-who-depends-on* lists dependent ASDF systems

And a general binding: **M-? or M-_** *slime-edit-uses** combines all
of the above, it lists every kind of references.

## Monitoring and controlling threads with Slime

**M-x slime-list-threads** (you can also access it through the
*slime-selector*, shortcut **t**) will list running threads by their
names, and their statuses.

The thread on the current line can be killed with **k**, or if there’s a
lot of threads to kill, several lines can be selected and **k** will kill
all the threads in the selected region.

**g** will update the thread list, but when you have a lot of threads
starting and stopping it may be too cumbersome to always press **g**, so
there’s a variable `slime-threads-update-interval`, when set to a number
X the thread list will be automatically updated each X seconds, a
reasonable value would be 0.5.
