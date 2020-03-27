---
title: "GUI Programming in Common Lisp, part 2/5: Qt4 with Qtools"
date: 2020-03-27T12:42:59+01:00
draft: false
---

Here's the second part of our exploration of GUI toolkits for Common Lisp.

The first part and introduction is accessible here:

- [part 1: Ltk](/blog/gui-programming-in-common-lisp-part-1-of-5-tk/)

**This blog post series was initially written for the Common Lisp
Cookbook, you can (and should) read it there:**

https://lispcookbook.github.io/cl-cookbook/gui.html

## Qt4 (Qtools)

Do we need to present Qt and [Qt4][qt4]? Qt is huge and contains
everything and the kitchen sink. Qt not only provides UI widgets, but
numerous other layers (networking, D-BUS…).

Qt is free for open-source software, however you'll want to check the
conditions to ship proprietary ones.

The [Qtools][qtools] bindings target Qt4. The Qt5 Lisp bindings are
yet to be created.

<!-- possible future: gobject-introspection -->

- **Framework written in**: C++
- **Framework Portability**: multi-platform, Android, embedded systems, WASM.
- **Bindings Portability**: Qtools runs on x86 desktop platforms on Windows, macOS and GNU/Linux.

- **Widgets choice**: large.

- **Graphical builder**: yes.

- **Other features**: Web browser, a lot more.

- **Bindings documentation**: lengthy explanations, a few examples. Prior Qt knowledge is required.
- **Bindings stability**: stable
- **Bindings activity**: active
- **Qt Licence**: both commercial and open source licences.
- Example applications:
  - https://github.com/Shinmera/qtools/tree/master/examples
  - https://github.com/Shirakumo/lionchat
  - https://github.com/shinmera/halftone - a simple image viewer

## Getting started

~~~lisp
(ql:quickload '(:qtools :qtcore :qtgui))
~~~

~~~lisp
(defpackage #:qtools-test
  (:use #:cl+qt)
  (:export #:main))
(in-package :qtools-test)
(in-readtable :qtools)
~~~

We create our main widget that will contain the rest:

~~~lisp
(define-widget main-window (QWidget)
  ())
~~~

We create an input field and a button inside this main widget:

~~~lisp
(define-subwidget (main-window name) (q+:make-qlineedit main-window)
  (setf (q+:placeholder-text name) "Your name please."))
~~~

~~~lisp
(define-subwidget (main-window go-button) (q+:make-qpushbutton "Go!" main-window))
~~~

We stack them horizontally:

~~~lisp
(define-subwidget (main-window layout) (q+:make-qhboxlayout main-window)
  (q+:add-widget layout name)
  (q+:add-widget layout go-button))
~~~

and we show them:

~~~lisp
(with-main-window
  (window 'main-window))
~~~

![](https://vindarel.github.io/cl-cookbook/assets/gui/qtools-intro.png)

That's cool, but we don't react to the click event yet.

### Reacting to events

Reacting to events in Qt happens through signals and slots. **Slots** are
functions that receive or "connect to" signals, and **signals** are event carriers.

Widgets already send their own signals: for example, a button sends a
"pressed" event. So, most of the time, we only need to connect to them.

However, had we extra needs, we can create our own set of signals.

#### Built-in events

We want to connect our `go-button` to the `pressed` and
`return-pressed` events and display a message box.

- we need to do this inside a `define-slot` function,
- where we establish the connection to those events,
- and where we create the message box. We grab the text of the `name`
  input field with `(q+:text name)`.

~~~lisp
(define-slot (main-window go-button) ()
  (declare (connected go-button (pressed)))
  (declare (connected name (return-pressed)))
  (q+:qmessagebox-information main-window
                              "Greetings"  ;; title
                              (format NIL "Good day to you, ~a!" (q+:text name))))
~~~

And voilà. Run it with

~~~lisp
(with-main-window (window 'main-window))
~~~

#### Custom events

We'll implement the same functionality as above, but for demonstration
purposes we'll create our own signal named `name-set` to throw when
the button is clicked.

We start by defining the signal, which happens inside the
`main-window`, and which is of type `string`:

~~~lisp
(define-signal (main-window name-set) (string))
~~~

We create a **first slot** to make our button react to the `pressed`
and `return-pressed` events. But instead of creating the message box
here, as above, we send the `name-set` signal, with the value of our
input field..

~~~lisp
(define-slot (main-window go-button) ()
  (declare (connected go-button (pressed)))
  (declare (connected name (return-pressed)))
  (signal! main-window (name-set string) (q+:text name)))
~~~

So far, nobody reacts to `name-set`. We create a **second slot** that
connects to it, and displays our message. Here again, we precise the
parameter type.

~~~lisp
(define-slot (main-window name-set) ((new-name string))
  (declare (connected main-window (name-set string)))
  (q+:qmessagebox-information main-window "Greetings" (format NIL "Good day to you, ~a!" new-name)))
~~~

and run it:

~~~lisp
(with-main-window (window 'main-window))
~~~

### Building and deployment

It is possible to build a binary and bundle it together with all the
necessary shared libraries.

Please read [https://github.com/Shinmera/qtools#deployment](https://github.com/Shinmera/qtools#deployment).

You might also like [this Travis CI script](https://github.com/phoe-trash/furcadia-post-splitter/blob/master/.travis.yml) to build a self-contained binary for the three OSes.

Next, we'll have a look at Gtk+3.


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
