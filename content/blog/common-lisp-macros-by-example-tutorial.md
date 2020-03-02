---
title: "Common Lisp Macros By Example Tutorial"
date: 2020-03-02T21:12:53+01:00
draft: false
---

I have recently edited and somewhat expanded the macros page on the
Common Lisp Cookbook. I find it may more legible and reader friendly,
so I reproduce it below (however, I cut two parts so than you get the essential).

You'd better read it on the Cookbook: https://lispcookbook.github.io/cl-cookbook/macros.html

---

The word _macro_ is used generally in computer science to mean a syntactic extension to a programming language. (Note: The name comes from the word "macro-instruction," which was a useful feature of many second-generation assembly languages. A macro-instruction looked like a single instruction, but expanded into a sequence of actual instructions. The basic idea has since been used many times, notably in the C preprocessor. The name "macro" is perhaps not ideal, since it connotes nothing relevant to what it names, but we're stuck with it.) Although many languages have a macro facility, none of them are as powerful as Lisp's. The basic mechanism of Lisp macros is simple, but has subtle complexities, so learning your way around it takes a bit of practice.

# How Macros Work

A macro is an ordinary piece of Lisp code that operates on _another piece of putative Lisp code,_ translating it into (a version closer to) executable Lisp. That may sound a bit complicated, so let's give a simple example. Suppose you want a version of [`setq`](http://www.lispworks.com/documentation/HyperSpec/Body/s_setq.htm) that sets two variables to the same value. So if you write

~~~lisp
(setq2 x y (+ z 3))
~~~

when `z`=8 then both `x` and `y` are set to 11. (I can't think of any use for this, but it's just an example.)

It should be obvious that we can't define `setq2` as a function. If `x`=50 and `y`=_-5_, this function would receive the values 50, _-5_, and 11; it would have no knowledge of what variables were supposed to be set. What we really want to say is, When you (the Lisp system) see:

```lisp
(setq2 v1 v2 e)
```

then treat it as equivalent to:

```lisp
(progn
  (setq v1 e)
  (setq v2 e))
```

Actually, this isn't quite right, but it will do for now. A macro allows us to do precisely this, by specifying a program for transforming the input pattern <code>(setq2 <i>v<sub>1</sub></i> <i>v<sub>2</sub></i> <i>e</i>)</code> into the output pattern `(progn ...)`.

## Quote

Here's how we could define the `setq2` macro:

~~~lisp
(defmacro setq2 (v1 v2 e)
  (list 'progn (list 'setq v1 e) (list 'setq v2 e)))
~~~

It takes as parameters two variables and one expression.

Then it returns a piece of code. In Lisp, because code is represented
as lists, we can simply return a list that represents code.

We also use the *quote*: each *quoted* symbol evaluates to itself, aka
it is returned as is:

- `(quote foo bar baz)` returns `(foo bar baz)`
- the quote character, `'`, is a shortcut for `quote`, a *special operator* (not a function nor a macro, but one of a few special operators forming the core of Lisp).
- so, `'foo` evaluates to `foo`.

So, our macro retourns the following bits:

- the symbol `progn`,
- a second list, that contains
  - the symbol `setq`
  - the variable `v1`: note that the variable is not evaluated inside the macro!
  - the expression `e`: it is not evaluated either!
- a second list, with `v2`.

We can use it like this:

```lisp
(defparameter v1 1)
(defparameter v2 2)
(setq2 v1 v2 3)
;; 3
```

We can check, `v1` and `v2` were set to `3`.


## Macroexpand

We must start writing a macro when we know what code we want to
generate. Once we've begun writing one, it becomes very useful to
check effectively what code does the macro generate. The function for
that is `macroexpand`. It is a function, and we give it some code, as
a list (so, we quote the code snippet we give it):

~~~lisp
(macroexpand '(setq2 v1 v2 3))
;; (PROGN (SETQ V1 3) (SETQ V2 3))
;; T
~~~

Yay, our macro expands to the code we wanted!

More interestingly:

~~~lisp
(macroexpand '(setq2 v1 v2 (+ z 3)))
;; (PROGN (SETQ V1 (+ z 3)) (SETQ V2 (+ z 3)))
;; T
~~~

We can confirm that our expression `e`, here `(+ z 3)`, was not
evaluated. We will see how to control the evaluation of arguments with
the comma: `,`.

Note: with Slime, you can call macroexpand by putting the cursor at
the left of the parenthesis of the s-expr to expand and call the function``M-x
slime-macroexpand-[1,all]``, or ``C-c M-m``:

~~~lisp
[|](setq2 v1 v2 3)
;^ cursor
; C-c M-m
; =>
; (PROGN (SETQ V1 3) (SETQ V2 3))
~~~


## Macros VS functions

Our macro is very close to the following function definition:

~~~lisp
(defun setq2-function (v1 v2 e)
  (list 'progn (list 'setq v1 e) (list 'setq v2 e)))
~~~

If we evaluated `(setq2-function 'x 'y '(+ z 3))` (note that each
argument is *quoted*, so it isn't evaluated when we call the
function), we would get

```lisp
(progn (setq x (+ z 3)) (setq y (+ z 3)))
```

This is a perfectly ordinary Lisp computation, whose sole point of interest is that its output is a piece of executable Lisp code. What `defmacro` does is create this function implicitly and make sure that whenever an expression of the form `(setq2 x y (+ z 3))` is seen, `setq2-function` is called with the pieces of the form as arguments, namely `x`, `y`, and `(+ z 3)`. The resulting piece of code then replaces the call to `setq2`, and execution resumes as if the new piece of code had occurred in the first place. The macro form is said to _expand_ into the new piece of code.

## Evaluation context

This is all there is to it, except, of course, for the myriad subtle consequences. The main consequence is that _run time for the `setq2` macro_ is _compile time for its context._ That is, suppose the Lisp system is compiling a function, and midway through it finds the expression `(setq2 x y (+ z 3))`. The job of the compiler is, of course, to translate source code into something executable, such as machine language or perhaps byte code. Hence it doesn't execute the source code, but operates on it in various mysterious ways. However, once the compiler sees the `setq2` expression, it must suddenly switch to executing the body of the `setq2` macro. As I said, this is an ordinary piece of Lisp code, which can in principle do anything any other piece of Lisp code can do. That means that when the compiler is running, the entire Lisp (run-time) system must be present.

We'll stress this once more: at compile-time, you have the full language at your disposal.

Novices often make the following sort of mistake. Suppose that the `setq2` macro needs to do some complex transformation on its `e` argument before plugging it into the result. Suppose this transformation can be written as a Lisp procedure `some-computation`. The novice will often write:

~~~lisp
(defmacro setq2 (v1 v2 e)
  (let ((e1 (some-computation e)))
    (list 'progn (list 'setq v1 e1) (list 'setq v2 e1))))

(defmacro some-computation (exp) ...) ;; _Wrong!_
~~~

The mistake is to suppose that once a macro is called, the Lisp system enters a "macro world," so naturally everything in that world must be defined using `defmacro`. This is the wrong picture. The right picture is that `defmacro` enables a step into the _ordinary Lisp world_, but in which the principal object of manipulation is Lisp code. Once that step is taken, one uses ordinary Lisp function definitions:

~~~lisp
(defmacro setq2 (v1 v2 e)
  (let ((e1 (some-computation e)))
    (list 'progn (list 'setq v1 e1) (list 'setq v2 e1))))

(defun some-computation (exp) ...) ;; _Right!_
~~~

One possible explanation for this mistake may be that in other languages, such as C, invoking a preprocessor macro _does_ get you into a different world; you can't run an arbitrary C program. It might be worth pausing to think about what it might mean to be able to.

Another subtle consequence is that we must spell out how the arguments to the macro get distributed to the hypothetical behind-the-scenes function (called `setq2-function` in my example). In most cases, it is easy to do so: In defining a macro, we use all the usual `lambda`-list syntax, such as `&optional`, `&rest`, `&key`, but what gets bound to the formal parameters are pieces of the macro form, not their values (which are mostly unknown, this being compile time for the macro form). So if we defined a macro thus:

~~~lisp
(defmacro foo (x &optional y &key (cxt 'null)) ...)
~~~

then

_If we call it thus ..._     |_The parameters' values are ..._
-----------------------------|-----------------------------------
`(foo a)`                    | `x=a`, `y=nil`, `cxt=null`
`(foo (+ a 1) (- y 1))`      |`x=(+ a 1)`, `y=(- y 1)`, `cxt=null`
`(foo a b :cxt (zap zip))`   |`x=a`, `y=b`, `cxt=(zap zip)`

<br>

Note that the values of the variables are the actual expressions `(+ a 1)` and `(zap zip)`. There is no requirement that these expressions' values be known, or even that they have values. The macro can do anything it likes with them. For instance, here's an even more useless variant of `setq`: <code>(setq-reversible <i>e<sub>1</sub></i> <i>e<sub>2</sub></i> <i>d</i>)</code> behaves like <code>(setq <i>e<sub>1</sub></i> <i>e<sub>2</sub></i>)</code> if <i>d=</i><code>:normal</code>, and behaves like <code>(setq <i>e<sub>2</sub></i> <i>e<sub>1</sub></i>)</code> if _d=_`:backward`. It could be defined thus:

~~~lisp
(defmacro setq-reversible (e1 e2 direction)
  (case direction
    (:normal (list 'setq e1 e2))
    (:backward (list 'setq e2 e1))
    (t (error "Unknown direction: ~a" direction))))
~~~

Here's how it expands:

~~~lisp
(macroexpand '(setq-reversible x y :normal))
(SETQ X Y)
T
(macroexpand '(setq-reversible x y :backward))
(SETQ Y X)
T
~~~

And with a wrong direction:

~~~lisp
(macroexpand '(setq-reversible x y :other-way-around))
~~~

We get an error and are prompted into the debugger!

We'll see the backquote and comma mechanism in the next section, but
here's a fix:

~~~lisp
(defmacro setq-reversible (v1 v2 direction)
  (case direction
    (:normal (list 'setq v1 v2))
    (:backward (list 'setq v2 v1))
    (t `(error "Unknown direction: ~a" ,direction))))
    ;; ^^ backquote                    ^^ comma: get the value inside the backquote.

(macroexpand '(SETQ-REVERSIBLE v1 v2 :other-way-around))
;; (ERROR "Unknown direction: ~a" :OTHER-WAY-AROUND)
;; T
~~~

Now when we call `(setq-reversible v1 v2 :other-way-around)` we still get the
error and the debugger, but at least not when using `macroexpand`.

<a name="2-backquote"></a>

# Backquote and comma

Before taking another step, we need to introduce a piece of Lisp notation that is indispensable to defining macros, even though technically it is quite independent of macros. This is the _backquote facility_. As we saw above, the main job of a macro, when all is said and done, is to define a piece of Lisp code, and that means evaluating expressions such as `(list 'prog (list 'setq ...) ...)`. As these expressions grow in complexity, it becomes hard to read them and write them. What we find ourselves wanting is a notation that provides the skeleton of an expression, with some of the pieces filled in with new expressions. That's what backquote provides. Instead of the the `list` expression given above, one writes

~~~lisp
  `(progn (setq ,v1 ,e) (setq ,v2 ,e))
;;^ backquote   ^   ^         ^   ^ commas
~~~

The backquote (`) character signals that in the expression that follows, every subexpression _not_ preceded by a comma is to be quoted, and every subexpression preceded by a comma is to be evaluated.

You can think of it, and use it, as data interpolation:

~~~lisp
`(v1 = ,v1) ;; => (V1 = 3)
~~~


That's mostly all there is to backquote. There are just two extra items to point out.

### Comma-splice ,@

First, if you write "`,@e`" instead of "`,e`" then the value of _e_ is _spliced_ (or "joined", "combined", "interleaved") into the result. So if `v` equals `(oh boy)`, then

~~~lisp
(zap ,@v ,v)
~~~

evaluates to

~~~lisp
(zap oh boy (oh boy))
;;   ^^^^^ elements of v (two elements), spliced.
;;          ^^ v itself (a list)
~~~

The second occurrence of `v` is replaced by its value. The first is replaced by the elements of its value. If `v` had had value `()`, it would have disappeared entirely: the value of `(zap ,@v ,v)` would have been `(zap ())`, which is the same as `(zap nil)`.

### Quote-comma ',

When we are inside a backquote context and we want to print an
expression literally, we have no choice but to use the combination of
quote and comma:

~~~lisp
(defmacro explain-exp (exp)
  `(format t "~S = ~S" ',exp ,exp))
  ;;                   ^^

(explain-exp (+ 2 3))
;; (+ 2 3) = 5
~~~

See by yourself:

~~~lisp
;; Defmacro with no quote at all:
(defmacro explain-exp (exp)
  (format t "~a = ~a" exp exp))
(explain-exp v1)
;; V1 = V1

;; OK, with a backquote and a comma to get the value of exp:
(defmacro explain-exp (exp)
  ;; WRONG example
  `(format t "~a = ~a" exp ,exp))
(explain-exp v1)
;; => error: The variable EXP is unbound.

;; We then must use quote-comma:
(defmacro explain-exp (exp)
  `(format t "~a = ~a" ',exp ,exp))
(explain-exp (+ 1 2))
;; (+ 1 2) = 3
~~~


### Nested backquotes

Second, one might wonder what happens if a backquote expression occurs inside another backquote. The answer is that the backquote becomes essentially unreadable and unwriteable; using nested backquote is usually a tedious debugging exercise. The reason, in my not-so-humble opinion, is that backquote is defined wrong. A comma pairs up with the innermost backquote when the default should be that it pairs up with the outermost. But this is not the place for a rant; consult your favorite Lisp reference for the exact behavior of nested backquote plus some examples.

### Building lists with backquote

[…]

# Getting Macros Right

I said in the first section that my definition of `setq2` wasn't quite right, and now it's time to fix it.

Suppose we write `(setq2 x y (+ x 2))`, when `x`_=8_. Then according to the definition given above, this form will expand into

~~~lisp
(progn
  (setq x (+ x 2))
  (setq y (+ x 2)))
~~~

so that `x` will have value 10 and `y` will have value 12. Indeed, here's its macroexpansion:

~~~lisp
(macroexpand '(setq2 x y (+ x 2)))
;;(PROGN (SETQ X (+ X 2)) (SETQ Y (+ X 2)))
~~~

Chances are that isn't what the macro is expected to do (although you never know). Another problematic case is `(setq2 x y (pop l))`, which causes `l` to be popped twice; again, probably not right.

The solution is to evaluate the expression `e` just once, save it in a temporary variable, and then set `v1` and `v2` to it.

## Gensym

To make temporary variables, we use the `gensym` function, which returns a fresh variable guaranteed to appear nowhere else. Here is what the macro should look like:

~~~lisp
(defmacro setq2 (v1 v2 e)
  (let ((tempvar (gensym)))
    `(let ((,tempvar ,e))
       (progn (setq ,v1 ,tempvar)
              (setq ,v2 ,tempvar)))))
~~~

Now `(setq2 x y (+ x 2))` expands to

~~~lisp
(let ((#:g2003 (+ x 2)))
  (progn (setq x #:g2003) (setq y #:g2003)))
~~~

Here `gensym` has returned the symbol `#:g2003`, which prints in this funny way because it won't be recognized by the reader. (Nor is there any need for the reader to recognize it, since it exists only long enough for the code that contains it to be compiled.)

Exercise: Verify that this new version works correctly for the case `(setq2 x y (pop l1))`.

Exercise: Try writing the new version of the macro without using backquote. If you can't do it, you have done the exercise correctly, and learned what backquote is for!

The moral of this section is to think carefully about which expressions in a macro get evaluated and when. Be on the lookout for situations where the same expression gets plugged into the output twice (as `e` was in my original macro design). For complex macros, watch out for cases where the order that expressions are evaluated differs from the order in which they are written. This is sure to trip up some user of the macro - even if you are the only user.<a name="LtohTOCentry-4"></a>

# What Macros are For

[…]

# See also

[A gentle introduction to Compile-Time Computing — Part 1](https://medium.com/@MartinCracauer/a-gentle-introduction-to-compile-time-computing-part-1-d4d96099cea0)

[Safely dealing with scientific units of variables at compile time (a gentle introduction to Compile-Time Computing — part 3)](https://medium.com/@MartinCracauer/a-gentle-introduction-to-compile-time-computing-part-3-scientific-units-8e41d8a727ca)

The following video, from the series
["Little bits of Lisp"](https://www.youtube.com/user/CBaggers/playlists)
by [cbaggers](https://github.com/cbaggers/), is a two hours long talk
on macros, showing simple to advanced concepts such as compiler
macros:
[https://www.youtube.com/watch?v=ygKXeLKhiTI](https://www.youtube.com/watch?v=ygKXeLKhiTI)
It also shows how to manipulate macros (and their expansion) in Emacs.

[![video](http://img.youtube.com/vi/ygKXeLKhiTI/0.jpg)](https://www.youtube.com/watch?v=ygKXeLKhiTI)
