---
title: "TIL how to interactively fix a failing test"
date: 2019-03-12T13:10:27+02:00
draft: false
---

I knew it was possible, but I got to try it recently.

Here I run a test with `fiveam`. It fails. I tell `fiveam` to enter
the debugger on failures with

    (setf 5am:*on-error* :debug)

so we have an immediate feedback and we can re-run the test from where
it left off by choosing the appropriate restart.

<iframe width="560" height="315" sandbox="allow-same-origin allow-scripts" src="https://peertube.video/videos/embed/c0c82209-feaa-444d-962f-afa25745bfc0" frameborder="0" allowfullscreen></iframe>

Other test frameworks like [Parachute](https://github.com/Shinmera/parachute) allow that.

This is one of the things that make development in Common Lisp
enjoyable and faster than with other workflows. Also, it's built-in,
there is no fancy editor plugin or configuration.


---

In the debugger:

- `<enter>` on a backtrace shows more of it
- `v` on a backtrace goes to the corresponding line or function.
- more options with the menu.
