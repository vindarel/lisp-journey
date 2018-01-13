+++
date = "2018-01-12T07:51:49+01:00"
title = "Generic, consistent and dotted access of data structures with Access"
draft = false
+++

A common frustration for (impatient) beginners is to see *different
function names* to access common data structures (alists, plists,
hash-tables) and their *inconsistencies* (the order of arguments).

Now they are well documented in the… Common Lisp Coobook of course:
https://lispcookbook.github.io/cl-cookbook/data-structures.html, but
still;

and it is annoying to try things out with a data structure and
*refactor* the code to use another one.

The library *[Access](https://github.com/AccelerationNet/access/)*
solves those problems, it's always

    (access my-var elt)


(if you're into this, note that [CL21 also does this with a generic and extensible `getf`](https://lispcookbook.github.io/cl-cookbook/cl21.html#generic-functions)).

edit: also [rutils](https://github.com/vseloved/rutils) with `generic-elt` or `?` in the `rutilsx` contrib package.

Access also solves another usecase.

Sometimes we deal with *nested data structures* (alist inside alist
inside alist, or mixed data structures, happens when working with an
API) and, as in other languages, we'd like a shortcut to access a
nested element. In Python, we can use `addict` to write
`foo.one.2.three` instead of `foo['one'][2]['three']`, with Access we
have two possibilities, see below.


Oh, and we can be confident *it is a battle-tested library*, since it
is the one that powers [Djula](https://github.com/mmontone/djula/)'s
template variables interplolation (doc is
[here](http://mmontone.github.io/djula/doc/build/html/variables.html)), where we can write

    {{ var.foo }}

à la Django for the supported data structures, and Djula is in the top 100 of the most downloaded Quicklisp libraries ([december 2017 stats](http://blog.quicklisp.org/2018/01/download-stats-for-december-2017.html)).


Let's install it:

~~~lisp
(ql:quickload "access")
~~~

import its symbols in Slime:

~~~lisp
(use-package :access)
~~~


## Generic and consistent access accross alists, plists, hash-tables, CLOS slots

Let's create our test variables first:

~~~lisp
(defparameter my-alist '((:foo . "foo") (:bar . "bar")))
MY-ALIST
~~~

~~~lisp
(defparameter my-plist (list :foo "foo" :bar "bar"))
MY-PLIST
~~~

~~~lisp
(defparameter my-hashtable (make-hash-table))
MY-HASHTABLE
(setf (gethash :foo my-hashtable) "foo")
"foo"
~~~

~~~lisp
(defclass obj-test ()
  ((foo :accessor foo :initarg :foo :initform :foo)
   (bar :accessor bar :initarg :bar :initform :bar)))
;; #<STANDARD-CLASS OBJ-TEST>
(defparameter my-obj (make-instance 'obj-test))
;; MY-OBJ
~~~

Now, let's access the `:foo` slot.

With **alists**:

~~~lisp
(access my-alist :foo)
"foo"
T
~~~

instead of `(cdr (assoc :foo my-alist))` (with :foo *first* argument) or alexandria's `(assoc-value my-alist :foo)` (:foo *second* argument).

**plists**:

~~~lisp
(access my-plist :foo)
"foo"
T
~~~

instead of `(getf my-plist :foo)` (unlike alists, with :foo as *last* argument).


**hash-tables**:

~~~lisp
(access my-hashtable :foo)
"foo"
T
~~~

instead of `(gethash :foo my-hashtable)` (:foo *first* argument).

**objects**:

~~~lisp
(access my-obj :foo) ;; <= accessor, not slot name
;; :FOO
;; T
~~~

instead of… it depends. Here we named the accessor `foo`, so we would have used simply `(foo my-obj)`.


Also note that `access` returns two values, the value and a boolean, t
if the slot exists, nil otherwise.

And `access` is `setf`able:

~~~lisp
(setf (access my-alist :foo) "oof")
~~~

### with-access

Below, we can bind temporary variables inside `with-access`:

~~~lisp
(with-access (foo bar (other-name plist))
             my-obj
           (format t "Got: ~a~a~a~&" foo bar other-name)
           ;; we can change variables
           (setf other-name "hello plist")
           (format t "other-name: ~a~&" other-name)
           ;; it changed the object too
           (format t "object slot: ~a~&" (plist my-obj)))
Got: FOOBAR(FOO foo BAR bar)
other-name: hello plist
object slot: hello plist
NIL
~~~



## Nested access

For this example we add a `plist` slot to our object, which copies our `my-plist` by default.

~~~lisp
(defclass obj-test ()
  ((foo :accessor foo :initarg :foo :initform :foo)
   (bar :accessor bar :initarg :bar :initform :bar)
   (plist :accessor plist :initarg plist :initform (copy-list MY-PLIST))))
#<STANDARD-CLASS OBJ-TEST>
~~~

(as being a CLOS object, `my-obj` is automatically updated with the new slot).

We can access the nested plist element `:foo` inside the object in one go with `accesses` (plurial):

~~~lisp
(accesses MY-OBJ 'plist :foo)
;; "foo"
~~~

instead of `(getf (plist my-obj) :foo)`.

## Dotted access

`with-dot` or `#D`

We can rewrite the previous examples with a dot:

~~~lisp
(with-dot ()
    my-alist.foo)
"foo"
~~~

or again


~~~lisp
(with-dot ()
    my-obj.foo)
"hello plist"
~~~

but even shorter, with the `#D` reader macro that we enable with

    (enable-dot-syntax)

(also works in Slime/Sly, for what I am not sure if I enabled a special feature)


~~~lisp
#Dmy-alist.foo
"foo"
~~~

and so, a *nested dotted access* through an object and a plist:

~~~lisp
;; back to initial case
(setf my-obj (make-instance 'obj-test))
;; #<OBJ-TEST {1005AA3B13}>
#Dmy-obj.plist.foo
;; "foo"
~~~

It will return `nil` instead of an error if someone in the middle
doesn't have the requested field.

---

Usage will tell how it is useful, and
I hope it will be to fellow newcomers.
