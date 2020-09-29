---
title: "Replic v0.12"
date: 2019-10-29T18:40:07+01:00
tags: ["libraries",]
draft: false
---

We recently pushed our [replic](https://github.com/vindarel/replic/)
library version 0.12, adding a couple of expected features, thanks to
the input of our ~~users~~ user:

- we can TAB-complete sentences (strings inside quotes)
- we can define a different completion method for each arguments of a
  command.
- we added a declarative way to automatically print a function's
  result. The default function can be overriden by users (in order
  too, for example, color output).

So we can do something like this: we create a function (that will
become a command on the readline command line):

~~~lisp
(defun say (verb name)
  (format t "~a, ~a !~&" verb name))
~~~

We define how to TAB-complete its arguments:

~~~lisp
(replic.completion:add-completion "say"
                                  (list "greetings" "\"nice to see you\"")
                                  (lambda () *names*))
~~~

Now if you type `say TAB` you get the two greeting choices. After you
pick one and press TAB again, you get the names that were given to
`hello`.

What's beginning to be wanted now is fuzzy completion.

Hope you enjoy.

---

What is replic's goal ?

> Building a readline application is cool, but readline gives you the
> basics and you must still build a REPL around it: loop and read
> commands, catch a =C-c=, a =C-d=, ask confirmation to quit, print
> the general help, the help of a command, setup the completion of
> commands, the completion of their arguments, load an init file,
> colorize output,...  =replic= does this for you.

Replic's goal is that when you have a lisp library, with lisp
functions, it should be straightforward to create a terminal
application out of it.

Here's an example in the wild. The `lyrics` library is cool. It is a lisp library, it must be used on the Lisp REPL. This is the amount of code that was needed to create a terminal application out of it: https://github.com/mihaiolteanu/lyrics/pull/1/files
