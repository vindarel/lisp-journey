---
title: "Example of interactively changing the current executed Lisp code in the debugger"
date: 2017-04-14T13:06:27+02:00
draft: false
tags: ["clojure", "debugging"]
---

The awesome example we will read comes from a comment by user lispm
inside a discussion on this reddit thread:
[https://www.reddit.com/r/programming/comments/65ct5j/a_pythonist_finds_a_new_home_at_clojure_land/](https://www.reddit.com/r/programming/comments/65ct5j/a_pythonist_finds_a_new_home_at_clojure_land/).

The article it discusses is a
["Not a monad tutorial"](https://notamonadtutorial.com/a-pythonist-finds-a-new-home-at-clojure-land-761ad8612b47)
post, where the interviewee is experienced in C++, Java, Javascript and Python and
turns into Clojure. He wrote about his first impressions with Common
Lisp
[here](https://facundoolano.wordpress.com/2012/01/31/first-impressions-on-common-lisp/),
where he raises usual concerns that I agree with but IMO that stay
supercifial ("not readable" because of stuff like `(format t
"~{~{~a:~10t~a~%~}~%~}" *db*)`, "huge operators set", "macros look
promising"…).

Here starts the discussion.

---

`dzecniv` On Common Lisp, I agree with the criticisms except

> the code was very difficult to read

I find it very easy, always well expressed, with concise
functions. And I find Clojure's harder, with more
`[`, `{` and the same number of other symbols (`#`, `*`).

Anyway, I'm in the process of trying to go from python to CL.
The CL ecosystem is quite good nowadays (equivalents of pip, venvs, pyenv, implementations (even
for the JVM or iOS), CI, sphinx, readthedocs, wsgi, setup.py,,…), it's moving, we can do quite a
lot ([awesome list](https://github.com/CodyReichert/awesome-cl)),
it has unique features but yeah, the ecosystem is tiny compared to
clojure's…

ps: interested ? http://lisp-lang.org/ !

`lispm`

> tiny compared to clojure

In many ways is it much broader than Clojure, since there is much more
choice. Interpreters, compilers, native code compilers, batch
compilers, compilers targeting
C/LLVM/JVM/ARM/ARM64/x86/x86-64/SPARC64/POWER/...

Clojure on the JVM uses a relatively simple and not very user-friendly
compiler to the JVM. No Interpreter. No mixed use of interpreted and
compiled code. Functions need to be declared before used. Error
messages are exposing the underlying JVM. No TCO. No images. Slow
startup.

The Roomba cleans your home with a CL program.

`MagicMurderBagYT`

> No Interpreter.

Hol up. What about the REPL?

`lispm`

That's not an interpreter. A REPL is not the same as a Lisp
interpreter. REPL means read eval print loop. EVAL can be implemented
by a compiler or an interpreter. Common Lisp has both and mixed
implementations with both compiler and interpreter.

A Lisp interpreter is executing Lisp code directly. Clojure does not have an Interpreter.

>    https://clojure.org/reference/evaluation

>    Clojure has no interpreter.

Example in LispWorks, which uses the Interpreter in the REPL:

~~~lisp
CL-USER 29 > (let ((f (lambda (a b)
                         (+ (prog1 2 (break))  ; we have a break here
                            (* a b)))))
              (funcall f 2 3))

Break.
  1 (continue) Return from break.
  2 (abort) Return to level 0.
  3 Return to top loop level 0.

Type :b for backtrace or :c <option number> to proceed.
Type :bug-form "<subject>" for a bug report template or :? for other options.
~~~


As you see Lisp comes with a sub-repl in the break. The sub-repl is
just another repl, but in the context of the break. The break could be
done by the debugger when it sees an error or by user code - as above.

Now we ask the interpreter for the current lambda expression:

~~~lisp
CL-USER 30 : 1 > :lambda
(LAMBDA (A B) (+ (PROG1 2 (BREAK)) (* A B)))
~~~

Above is actually Lisp data. Code as data.

Now I'm changing the + function in the code to be expt,
exponentiation. To be clear: I'm changing in the debugger the current
executed Lisp function on the Lisp level. We take the third element of
the list, and then the first one of that. This is the + symbol. We
change it to be expt. * holds the last evaluation result of the REPL.

~~~lisp
CL-USER 31 : 1 > (setf (first (third *)) 'expt)
EXPT
~~~

Then I'm restarting the current stack frame:

~~~lisp
CL-USER 32 : 1 > :res
~~~

We get another break, which we just continue from:

```
Break.
  1 (continue) Return from break.
  2 (abort) Return to level 0.
  3 Return to top loop level 0.

Type :b for backtrace or :c <option number> to proceed.
Type :bug-form "<subject>" for a bug report template or :? for other options.

CL-USER 33 : 1 > :c 1
64                                   ; we computed 2^(2*3)  instead of 2+(2*3)
```

What did we see? We saw that the interpreter uses actual Lisp
code. Lisp code we can change with Lisp code in the debugger.

A second example.

What can we do with that for debugging? Well, we can for example write
our own evaluation tracer. The Evaluator prints each expression and
its result nicely indented, while walking the expression tree and
evaluating subexpressions. Remember: this is now user-level code. The
example is from CLtL2. You will also see that LispWorks can freely mix
compiled and interpreted functions. The function COMPILE takes a
function name and compiles its Lisp code to machine code.

~~~lisp
CL-USER 1 > (defvar *hooklevel* 0)
*HOOKLEVEL*

CL-USER 2 > (defun hook (x)
              (let ((*evalhook* 'eval-hook-function))
                (eval x)))
HOOK

CL-USER 3 > (compile 'hook)
HOOK
NIL
NIL

CL-USER 4 > (defun eval-hook-function (form &rest env)
              (let ((*hooklevel* (+ *hooklevel* 1)))
                (format *trace-output* "~%~V@TForm:  ~S"
                        (* *hooklevel* 2) form)
                (let ((values (multiple-value-list
                               (evalhook form
                                         #'eval-hook-function
                                         nil
                                         env))))
                  (format *trace-output* "~%~V@TValue:~{ ~S~}"
                          (* *hooklevel* 2) values)
                  (values-list values))))
EVAL-HOOK-FUNCTION

CL-USER 5 > (compile 'eval-hook-function)
EVAL-HOOK-FUNCTION
NIL
NIL
~~~

Now we can trace the evaluation of expressions on the Lisp level:

```
CL-USER 6 > (hook '(cons (floor *print-base* 2) 'b))

  Form:  (CONS (FLOOR *PRINT-BASE* 2) (QUOTE B))
    Form:  (FLOOR *PRINT-BASE* 2)
      Form:  *PRINT-BASE*
      Value: 10
      Form:  2
      Value: 2
    Value: 5 0
    Form:  (QUOTE B)
    Value: B
  Value: (5 . B)
(5 . B)
```

`dzecniv`

that's an awesome example and tutorial that I'd love to see on a blog
post or just a gist or something for further reference and better
archiving, this will be buried too quickly on reddit !

---

So here it is.

Epilogue: [the Roomba robot vacuums](https://duckduckgo.com/?q=the+roomba&ia=web).
