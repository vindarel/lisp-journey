---
title: "GUI Programming in Common Lisp, part 5/5: Nuklear"
date: 2020-03-27T13:19:45+01:00
draft: false
---

[Nuklear][nuklear] is a small [immediate-mode](https://en.wikipedia.org/wiki/Immediate_mode_GUI) GUI toolkit:

> [Nuklear] is a minimal-state, immediate-mode graphical user interface toolkit written in ANSI C and licensed under public domain. It was designed as a simple embeddable user interface for application and does not have any dependencies, a default render backend or OS window/input handling but instead provides a highly modular, library-based approach, with simple input state for input and draw commands describing primitive shapes as output. So instead of providing a layered library that tries to abstract over a number of platform and render backends, it focuses only on the actual UI.

its Lisp binding is [Bodge-Nuklear][bodge-nuklear], and its higher level companions [bodge-ui](https://github.com/borodust/bodge-ui) and [bodge-ui-window](https://github.com/borodust/bodge-ui-window).

Unlike traditional UI frameworks, Nuklear allows the developer to take
over the rendering loop or the input management. This might require
more setup, but it makes Nuklear particularly well suited for games,
or for applications where you want to create new controls.

Previous posts of the serie:

- [part 1: Ltk](/blog/gui-programming-in-common-lisp-part-1-of-5-tk/)
- [part 2: Qt4](/blog/gui-programming-in-common-lisp-part-2-of-5-qt4-qtools/)
- [part 3: Gtk+3](/blog/gui-programming-in-common-lisp-part-3-of-5-gtk3/)
- [part 4: IUP](/blog/gui-programming-in-common-lisp-part-4-of-5-iup/)

**This blog post series was initially written for the Common Lisp
Cookbook, you can (and should) read it there:**

https://lispcookbook.github.io/cl-cookbook/gui.html

- **Framework written in**: ANSI C, single-header library.
- **Portability**: where C runs. Nuklear doesn't contain
  platform-specific code. No direct OS or window handling is done in
  Nuklear. Instead *all input state has to be provided by platform
  specific code*.

- **Widgets choice**: small.

- **Graphical builder**: no.

- **Other features**: fully skinnable and customisable.

- **Bindings stability**: stable
- **Bindings activity**: active
- **Licence**: MIT or Public Domain (unlicence).
- Example applications:
  - [Trivial-gamekit](https://github.com/borodust/trivial-gamekit)
  - [Obvius](https://github.com/thicksteadTHpp/Obvius/) - a resurrected image processing library.
  - [Notalone](https://github.com/borodust/notalone) - an autumn 2017 Lisp Game Jam entry.

**List of widgets**

Non-exhaustive list:

```
buttons, progressbar, image selector, (collapsable) tree, list, grid, range, slider, color picker,
date-picker
```

![](https://vindarel.github.io/cl-cookbook/assets/gui/nuklear.png)

## Getting started

**Disclaimer**: as per the author's words at the time of writing,
bodge-ui is in early stages of development and not ready for general
use yet. There are some quirks that need to be fixed, which might
require some changes in the API.

`bodge-ui` is not in Quicklisp but in its own Quicklisp distribution. Let's install it:

~~~lisp
(ql-dist:install-dist "http://bodge.borodust.org/dist/org.borodust.bodge.txt" :replace t :prompt nil)
~~~

Uncomment and evaluate this line only if you want to enable the OpenGL 2
renderer:

~~~lisp
;; (cl:pushnew :bodge-gl2 cl:*features*)
~~~

Quickload `bodge-ui-window`:

~~~lisp
(ql:quickload :bodge-ui-window)
~~~

We can run the built-in example:

~~~lisp
(ql:quickload :bodge-ui-window/examples)
(bodge-ui-window.example.basic:run)
~~~

Now let's define a package to write a simple application.

~~~lisp
(cl:defpackage :bodge-ui-window-test
  (:use :cl :bodge-ui :bodge-host))
(in-package :bodge-ui-window-test)
~~~

~~~lisp
(defpanel (main-panel
            (:title "Hello Bodge UI")
            (:origin 200 50)
            (:width 400) (:height 400)
            (:options :movable :resizable
                      :minimizable :scrollable
                      :closable))
  (label :text "Nested widgets:")
  (horizontal-layout
   (radio-group
    (radio :label "Option 1")
    (radio :label "Option 2" :activated t))
   (vertical-layout
    (check-box :label "Check 1" :width 100)
    (check-box :label "Check 2"))
   (vertical-layout
    (label :text "Awesomely" :align :left)
    (label :text "Stacked" :align :centered)
    (label :text "Labels" :align :right)))
  (label :text "Expand by width:")
  (horizontal-layout
   (button :label "Dynamic")
   (button :label "Min-Width" :width 80)
   (button :label "Fixed-Width" :expandable nil :width 100))
  )

(defun run ()
  (bodge-host:open-window (make-instance 'main-window)))
~~~

and run it:

~~~lisp
(run)
~~~

![](https://vindarel.github.io/cl-cookbook/assets/gui/nuklear-test.png)

To react to events, use the following signals:

```
:on-click
:on-hover
:on-leave
:on-change
:on-mouse-press
:on-mouse-release
```

They take as argument a function with one argument, the panel. But
beware: they will be called on each rendering cycle when the widget is
on the given state, so potentially a lot of times.


### Interactive development

If you ran the example in the REPL, you couldn't see what's cool. Put
the code in a lisp file and run it, so than you get the window. Now
you can change the panel widgets and the layout, and your changes will
be immediately applied while the application is running!


# Conclusion

Have fun, don't hesitate to share your experience and your apps… and contribute to the Cookbook!


[tk]: https://www.tcl.tk
[ltk]: http://www.peter-herth.de/ltk/ltkdoc/
[qt4]: https://doc.qt.io/archives/qt-4.8/index.html
[gtk]: https://www.gtk.org/
[qtools]: https://github.com/Shinmera/qtools
[cl-cffi-gtk]: https://github.com/Ferada/cl-cffi-gtk/
[iup-tecgraf]: http://webserver2.tecgraf.puc-rio.br/iup/
[iup-lisp]: https://github.com/lispnik/iup/
[gnome]: https://www.gnome.org/
[nuklear]: https://github.com/Immediate-Mode-UI/Nuklear
[bodge-nuklear]: https://github.com/borodust/bodge-nuklear
[capi]: http://www.lispworks.com/products/capi.html
[ceramic]: http://ceramic.github.io/
