---
title: "Compile Time Type Checking in Common Lisp"
date: 2019-10-29T17:12:33+01:00
tags: ["tutorial",]
draft: false
---

We often hear that Common Lisp is dynamically typed, which is not
wrong, but that leads to the belief that Lisp is as bad as Python
concerning types, which is plainly wrong. We don't hear enough that CL
is a compiled language, that we can add type annotations, and that
SBCL does thorough type checking. Hence, what we have at hand is
awesome: we can compile a whole program or *compile a single function*
and get type warnings. Once again, the feedback is immediate. We can
define our own types and get compile-time type warnings.

You use a paramater that must be a list of list of strings of length 3
? Ok, define the type:

~~~lisp
(defun list-of-3tuples-strings-p (list)
  "Return t if LIST is a list composed of 3-tuples, made only of strings."
  (and (consp list)
       (every (lambda (it)
                (and
                 (= 3 (length it))
                 (every #'stringp it)))
              list)))

(deftype alist-of-3tuples-strings ()
  `(satisfies list-of-3tuples-strings-p))
~~~

and type the variable as explained below.

It's useful for development, it's also great to catch errors in a
user's configuration file. Checks are done when we `load` a file, and
error messages are explicit. We use this now in the Next browser.

We don't hear a lot about all that, maybe because the information was
hard to find, maybe because SBCL was not there at the time Lisp books
were written. The following was published to the [Common Lisp
Cookbook /type.html](https://lispcookbook.github.io/cl-cookbook/type.html), so
hopefully the issue is solved!

On the topic, don't miss these:

- the article [Static type checking in SBCL](https://medium.com/@MartinCracauer/static-type-checking-in-the-programmable-programming-language-lisp-79bb79eb068a), by Martin Cracauer
- the article [Typed List, a Primer](https://alhassy.github.io/TypedLisp/) - let's explore Lisp's fine-grained type hierarchy! with a shallow comparison to Haskell.
- the [Coalton](https://github.com/stylewarning/coalton) library
  (pre-alpha): adding Hindley-Milner type checking to Common Lisp
  which allows for gradual adoption, in the same way Typed Racket or
  Hack allows for. It is as an embedded DSL in Lisp that resembles
  Standard ML or OCaml, but lets you seamlessly interoperate with
  non-statically-typed Lisp code (and vice versa).


## Compile-time type checking

You may provide type information for variables, function arguments
etc via the macros [`declare`][declare] and [`declaim`][declaim].
However, similar to the `:type` slot
introduced in [CLOS section][clos], the effects of type declarations are
undefined in Lisp standard and are implementation specific. So there is no
guarantee that the Lisp compiler will perform compile-time type checking.

However, it is possible, and SBCL is an implementation that does
thorough type checking.

Let's recall first that Lisp already warns about simple type
warnings. The following function wrongly wants to concatenate a string
and a number. When we compile it, we get a type warning.

~~~lisp
(defconstant +foo+ 3)
(defun bar ()
  (concatenate 'string "+" +foo+))
; caught WARNING:
;   Constant 3 conflicts with its asserted type SEQUENCE.
;   See also:
;     The SBCL Manual, Node "Handling of Types"
~~~

The example is simple, but it already shows a capacity some other
languages don't have, and it is actually useful during development ;)
Now, we'll do better.


### Declaring the type of variables

Use the macro [`declaim`][declaim].

Let's declare that our global variable `*name*` is a string (you can
type the following in any order in the REPL):

~~~lisp
(declaim (type (string) *name*))
(defparameter *name* "book")
~~~

Now if we try to set it with a bad type, we get a `simple-type-error`:

~~~lisp
(setf *name* :me)
Value of :ME in (THE STRING :ME) is :ME, not a STRING.
   [Condition of type SIMPLE-TYPE-ERROR]
~~~

We can do the same with our custom types. Let's quickly declare the type `list-of-strings`:

~~~lisp
(defun list-of-strings-p (list)
  "Return t if LIST is non nil and contains only strings."
  (and (consp list)
       (every #'stringp list)))

(deftype list-of-strings ()
  `(satisfies list-of-strings-p))
~~~

Now let's declare that our `*all-names*` variables is a list of strings:

~~~lisp
(declaim (type (list-of-strings) *all-names*))
(defparameter *all-names* "")
~~~

We can compose types:

~~~lisp
(declaim (type (or null list-of-strings) *all-names*))
~~~

### Declaring the input and output types of functions

We use again the `declaim` macro, with `ftype (function …)` instead of just `type`:

~~~lisp
(declaim (ftype (function (fixnum) fixnum) add))
;;                         ^^input ^^output [optional]
(defun add (n)
	(+ n  1))
~~~

With this we get nice type warnings at compile time.

If we change the function to erroneously return a string instead of a
fixnum, we get a warning:

~~~lisp
(defun add (n)
	(format nil "~a" (+ n  1)))
; caught WARNING:
;   Derived type of ((GET-OUTPUT-STREAM-STRING STREAM)) is
;     (VALUES SIMPLE-STRING &OPTIONAL),
;   conflicting with the declared function return type
;     (VALUES FIXNUM &REST T).
~~~

If we use `add` inside another function, to a place that expects a
string, we get a warning:

~~~lisp
(defun bad-concat (n)
    (concatenate 'string (add n)))
; caught WARNING:
;   Derived type of (ADD N) is
;     (VALUES FIXNUM &REST T),
;   conflicting with its asserted type
;     SEQUENCE.
~~~

If we use `add` inside another function, and that function declares
its argument types which appear to be incompatible with those of
`add`, we get a warning:

~~~lisp
(declaim (ftype (function (string)) bad-arg))
(defun bad-arg (n)
    (add n))
; caught WARNING:
;   Derived type of N is
;     (VALUES STRING &OPTIONAL),
;   conflicting with its asserted type
;     FIXNUM.
~~~

This all happens indeed *at compile time*, either in the REPL,
either with a simple `C-c C-c` in Slime, or when we `load` a file.

[defvar]: http://www.lispworks.com/documentation/lw51/CLHS/Body/m_defpar.htm
[setf]: http://www.lispworks.com/documentation/lw50/CLHS/Body/m_setf_.htm
[type-of]: http://www.lispworks.com/documentation/HyperSpec/Body/f_tp_of.htm
[type-specifiers]: http://www.lispworks.com/documentation/lw51/CLHS/Body/04_bc.htm
[number]: http://www.lispworks.com/documentation/lw61/CLHS/Body/t_number.htm
[typep]: http://www.lispworks.com/documentation/lw51/CLHS/Body/f_typep.htm
[subtypep]: http://www.lispworks.com/documentation/lw71/CLHS/Body/f_subtpp.htm
[string]: http://www.lispworks.com/documentation/lw71/LW/html/lw-426.htm
[simple-array]: http://www.lispworks.com/documentation/lw70/CLHS/Body/t_smp_ar.htm
[integer]: http://www.lispworks.com/documentation/lw71/CLHS/Body/t_intege.htm
[describe]: http://www.lispworks.com/documentation/lw51/CLHS/Body/f_descri.htm
[clos]: clos.html
[character]: http://www.lispworks.com/documentation/lcl50/ics/ics-14.html
[number]: http://www.lispworks.com/documentation/lw61/CLHS/Body/t_number.htm
[complex]: http://www.lispworks.com/documentation/lw70/CLHS/Body/t_comple.htm
[real]: http://www.lispworks.com/documentation/lw70/CLHS/Body/t_real.htm
[rational]: http://www.lispworks.com/documentation/HyperSpec/Body/t_ration.htm
[class-of]: http://www.lispworks.com/documentation/HyperSpec/Body/f_clas_1.htm
[typecase]: http://www.lispworks.com/documentation/lw60/CLHS/Body/m_tpcase.htm
[deftype]: http://www.lispworks.com/documentation/lw51/CLHS/Body/m_deftp.htm
[defmacro]: http://www.lispworks.com/documentation/lw70/CLHS/Body/m_defmac.htm
[check-type]: http://www.lispworks.com/documentation/HyperSpec/Body/m_check_.htm#check-type
[type-error]: http://www.lispworks.com/documentation/HyperSpec/Body/e_tp_err.htm#type-error
[place]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_p.htm#place
[declare]: http://www.lispworks.com/documentation/HyperSpec/Body/s_declar.htm
[declaim]: https://www.xach.com/clhs?q=declaim
[safety]: http://www.lispworks.com/documentation/HyperSpec/Body/d_optimi.htm#speed
