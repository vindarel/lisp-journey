--- title: "GUI Programming in Common Lisp, part 1/5: Tk"
date: 2020-03-27T12:20:36+01:00
draft: false
---

Lisp has a long and rich history and so does the development of
Graphical User Interfaces in Lisp. In fact, the first GUI builder was
written in Lisp (and sold to Apple. It is now Interface Builder).

Lisp is also famous and unrivaled for its interactive development
capabilities, a feature even more worth having to develop GUI
applications. Can you imagine compiling one function and seeing your
GUI update instantly? We can do this with many GUI frameworks today,
even though the details differ from one to another.

Finally, a key part in building software is how to build it and ship
it to users. Here also, we can build self-contained binaries, for
the three main operating systems, that users can run with a double
click.

We aim here to give you the relevant information to help you choose
the right GUI framework and to put you on tracks. Don't hesitate to
[contribute](https://github.com/LispCookbook/cl-cookbook/issues/), to
send more examples and to furnish the upstream documentations.

**This blog post series was initially written for the Common Lisp
Cookbook, you can (and should) read it there:**

https://lispcookbook.github.io/cl-cookbook/gui.html

# Introduction

In this article series, we'll present the following GUI toolkits:

- [Tk][tk] with [Ltk][ltk]
- [Qt4][qt4] with [Qtools][qtools]
- [IUP][iup-tecgraf] with [lispnik/iup][iup-lisp]
- [Gtk3][gtk] with [cl-cffi-gtk][cl-cffi-gtk]
- [Nuklear][nuklear] with [Bodge-Nuklear][bodge-nuklear]

In addition, you might want to have a look to:

- the [CAPI][capi] toolkit (Common Application Programming Interface),
  which is proprietary and made by LispWorks. It is a complete and cross-platform
  toolkit (Windows, Gtk+, Cocoa), very praised by its users. LispWorks
  also has [iOS and Android
  runtimes](http://www.lispworks.com/products/lw4mr.html). Example
  software built with CAPI include [Opusmodus](https://opusmodus.com/)
  or again [ScoreCloud](https://scorecloud.com/). It is possible to
  try it with the LispWorks free demo.
- [CocoaInterface](https://github.com/plkrueger/CocoaInterface/), a
Cocoa interface for Clozure Common Lisp. Build Cocoa user interface
windows dynamically using Lisp code and bypass the typical Xcode
processes.
* [McCLIM](https://common-lisp.net/project/mcclim/), a toolkit in 100% Common Lisp.
* [Alloy](https://github.com/Shirakumo/alloy), another very new toolkit in 100% Common Lisp, used for example in the [Kandria](https://github.com/shinmera/kandria) game.
* [nodgui](https://notabug.org/cage/nodgui), a fork of Ltk, with syntax sugar and additional widgets.
* [eql, eql5, eql5-android](https://gitlab.com/eql), embedded Qt4 and Qt5 Lisp, embedded in ECL, embeddable in Qt. Port of EQL5 to the Android platform.
* this [demo using Java Swing from ABCL](https://github.com/defunkydrummer/abcl-jazz)
* and, last but not least, [Ceramic][ceramic], to ship a cross-platform web app with Electron.

as well as the other ones listed on [awesome-cl#gui](https://github.com/CodyReichert/awesome-cl#gui) and [Cliki](https://www.cliki.net/GUI).

# Tk (Ltk)

[Tk][tk] (or Tcl/Tk, where Tcl is the programming language) has the
infamous reputation of having an outdated look. This is not (so) true
anymore since its version 8 of 1997 (!). It is probably better than
you think:

![Ltk looks ok now](https://vindarel.github.io/cl-cookbook/assets/gui/ltk-on-macos.png)

Tk doesn't have a great choice of widgets, but it has a useful canvas,
and it has a couple of unique features: we can develop a graphical
interface **fully interactively** and we can run the GUI **remotely**
from the core app.

So, Tk isn't fancy, but it is an used and proven GUI toolkit (and
programming language) still used in the industry. It can be a great
choice to quickly create simple GUIs, to leverage its ease of deployment, or
when stability is required.

The Lisp binding is [Ltk][ltk].

- **Written in**: Tcl
- **Portability**: cross-platform (Windows, macOS, Linux).

- **Widgets**: this is not the fort of Tk. It has a **small set** of
  default widgets, and misses important ones, for example a calendar. We
  can find some in extensions (such as in **Nodgui**), but they don't
  feel native, at all.

- **Interactive development**: very much.

- **Graphical builder**: no

- **Other features**:
  - **remote execution**: the connection between Lisp and Tcl/Tk is
    done via a stream. It is thus possible to run the Lisp program on
    one computer, and to display the GUI on another one. The only
    thing required on the client computer is tcl/tk installed and the
    remote.tcl script. See [Ltk-remote](http://www.peter-herth.de/ltk/ltkdoc/node46.html).

- **Bindings documentation**: short but complete. Nodgui too.
- **Bindings stability**: very stable
- **Bindings activity**: low to non-existent.
- **Licence**: Tcl/Tk is BSD-style, Ltk is LGPL.
- Example applications:
  - [Fulci](https://notabug.org/cage/fulci/) - a program to organize your movie collections.
  - [Ltk small games](https://github.com/mijohnson99/ltk-small-games) - snake and tic-tac-toe.
  - [cl-torrents](https://github.com/vindarel/cl-torrents) - searching torrents on popular trackers. CLI, readline and a simple Tk GUI.


**List of widgets**

(please don't suppose the list exhaustive)

```
Button Canvas Check-button Entry Frame Label Labelframe Listbox
Menu Menubutton Message
Paned-window
Radio-button Scale
Scrollbar Spinbox Text
Toplevel Widget Canvas

Ltk-megawidgets:
    progress
    history-entry
    menu-entry
```

Nodgui adds:

```
treelist tooltip searchable-listbox date-picker calendar autocomplete-listbox
password-entry progress-bar-star notify-window
dot-plot bar-chart equalizer-bar
swap-list
```

# Getting started

Ltk is quick and easy to grasp.

~~~lisp
(ql:quickload :ltk)
(in-package :ltk-user)
~~~


**How to create widgets**

All widgets are created with a regular `make-instance` and the widget name:

~~~lisp
(make-instance 'button)
(make-instance 'treeview)
~~~

This makes Ltk explorable with the default symbol completion.

**How to start the main loop**

As with most bindings, the GUI-related code must be started inside a macro that
handles the main loop, here `with-ltk`:

~~~lisp
(with-ltk ()
  (let ((frame (make-instance 'frame)))
    …))
~~~

**How to display widgets**

After we created some widgets, we must place them on the layout. There
are a few Tk systems for that, but the most recent one and the one we
should start with is the `grid`. `grid` is a function that takes as
arguments the widget, its column, its row, and a few optional
parameters.

As with any Lisp code in a regular environment, the functions'
signatures are indicated by the editor. It makes Ltk explorable.

Here's how to display a button:

~~~lisp
(with-ltk ()
  (let ((button (make-instance 'button :text "hello")))
    (grid button 0 0)))
~~~

That's all there is to it.


### Reacting to events

Many widgets have a `:command` argument that accept a lambda which is
executed when the widget's event is started. In the case of a button,
that will be on a click:

~~~lisp
(make-instance 'button
  :text "Hello"
  :command (lambda ()
             (format t "clicked")))
~~~


### Interactive development

When we start the Tk process in the background with `(start-wish)`, we
can create widgets and place them on the grid interactively.

See [the documentation](http://www.peter-herth.de/ltk/ltkdoc/node8.html).

Once we're done, we can `(exit-wish)`.


### Nodgui

To try the Nodgui demo, do:

~~~lisp
(ql:quickload :nodgui)
(nodgui.demo:demo)
~~~

Next, we'll have a look at a very different beast, Qt4, with Qtools.


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
