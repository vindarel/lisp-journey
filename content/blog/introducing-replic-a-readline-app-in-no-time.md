---
title: "Introducing Replic: an executable and a library to build a readline app in no time"
date: 2019-01-09T14:41:57+01:00
draft: false
---

When I started dabbling in CL, I tried to build a readline application
to see how it goes. I found
[cl-readline](https://github.com/vindarel/cl-readline) (I'm only the
new maintainer) and it went smoothly. So I built a second and a third
app, and found many things to refactor and provide out of the box: now
comes [replic](https://github.com/vindarel/replic/).

It comes as a library (now in Quicklisp, since 2018-01) and as an
executable. The library does the following for you:

- it builds the repl loop, catches a C-c, a C-d, errors,
- it asks confirmation to quit,
- it asks depending on a .conf and a lispy config file,
- it reads parameters from a config file,
- it prints the help of all or one command (with optional highlighting),
- and more importantly it handles the completion of commands and of their arguments.

For example, instead of this "repl" loop:

~~~lisp
  (handler-case
      (do ((i 0 (1+ i))
           (text "")
           (verb "")
           (function nil)
           (variable nil)
           (args ""))
          ((string= "quit" (str:trim text)))

        (handler-case
            (setf text
                  (rl:readline :prompt (prompt)
                               :add-history t))
          (#+sbcl sb-sys:interactive-interrupt ()
                  (progn
                    (when (confirm)
                      (uiop:quit)))))

        (if (string= text "NIL")
            ;; that's a C-d, a blank input is just "".
            (when (confirm)
              (uiop:quit)))

        (unless (str:blank? text)
          (setf verb (first (str:words text)))
          (setf function (if (replic.completion:is-function verb)
                             ;; might do better than this or.
                             (replic.completion:get-function verb)))
          (setf variable (if (replic.completion:is-variable verb)
                             (replic.completion:get-variable verb)))
          (setf args (rest (str:words text)))


          (if (and verb function)
              (handler-case
                  ;; Call the function.
                  (apply function args)
                (#+sbcl sb-sys:interactive-interrupt (c)
                        (declare (ignore c))
                        (terpri))
                (error (c) (format t "Error: ~a~&" c)))

              (if variable
                  (format t "~a~&" (symbol-value variable))
                  (format t "No command or variable bound to ~a~&" verb)))

          (finish-output)

          (when (and *history*
                     *write-history*)
            (rl:write-history "/tmp/readline_history"))
          ))

    (error (c)
      (format t "~&Unknown error: ~&~a~&" c)))
~~~

you call:

~~~lisp
(replic:repl)
~~~

To turn all exported functions of a package into commands, use

    (replic:functions-to-commands :my-package)

and you can find them into the readline app.


Setting the completion of commands is easy, we use
`(replic.completion:add-completion "my-function" <list-or-lambda>`. For example:

~~~lisp
(in-package :replic.user)

(defparameter *names* '()
  "List of names (string) given to `hello`. Will be autocompleted by `goodbye`.")

(defun hello (name)
  "Takes only one argument. Adds the given name to the global
  `*names*` variable, used to complete arguments of `goodbye`.
  "
  (format t "hello ~a~&" name)
  (push name *names*))

(defun goodbye (name)
  "Says goodbye to name, where `name` should be completed from what was given to `hello`."
  (format t "goodbye ~a~&" name))

(replic.completion:add-completion "goodbye" (lambda () *names*))

(export '(hello goodbye))
~~~


This example can be used with the *executable*. What it does is read
your code from a lisp file (`~/.replic.lisp` or an argument on the
command line) and it turns the exported functions into commands, for
which we can specify custom completion.

For more details, see the readme.

---

I use this currently in three apps of mine (like
[cl-torrents](https://github.com/vindarel/cl-torrents)). It's
simple. It could be more: it could infer the arguments' type, do fuzzy
completion, maybe integrate a Lisp editor (Lem) or a lispy shell
(shcl), separate the commands in apps, expose hooks, have a set of
built-in shell related utilities, highlight the input line, it could
be web-based,…

For now it's going smoothly.

I'll finish by recalling that it's amazing to be able to ship
self-contained executables to users !
