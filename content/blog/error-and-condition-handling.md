+++
date = "2018-01-02T15:51:49+01:00"
title = "Error and condition handling"
tags = ["tutorial",]
draft = false
+++

Common Lisp has mechanisms for error and condition handling as found
in other languages, and can do more.

What is a condition ?

> Just like in languages that support exception handling (Java, C++,
> Python, etc.), a condition represents, for the most part, an
> “exceptional” situation. However, even more so that those languages,
> *a condition in Common Lisp can represent a general situation where
> some branching in program logic needs to take place*, not
> necessarily due to some error condition. Due to the highly
> interactive nature of Lisp development (the Lisp image in
> conjunction with the REPL), this makes perfect sense in a language
> like Lisp rather than say, a language like Java or even Python,
> which has a very primitive REPL. In most cases, however, we may not
> need (or even allow) the interactivity that this system offers
> us. Thankfully, the same system works just as well even in
> non-interactive mode.
>
> [z0ltan](https://z0ltan.wordpress.com/2016/08/06/conditions-and-restarts-in-common-lisp/)


Let's dive into it step by step. More resources are given afterwards.


Now *[best read in the Common Lisp Cookbook](https://lispcookbook.github.io/cl-cookbook/error_handling.html)*.

Credit: our [cl-torrents tutorial](https://vindarel.github.io/cl-torrents/tutorial.html).

Note: you can contribute any fix or addition to the Cookbook or this page via git ;)


<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [Ignore all errors (and return nil)](#ignore-all-errors-and-return-nil)
- [Catching any condition - handler-case](#catching-any-condition---handler-case)
- [Catching a specific condition](#catching-a-specific-condition)
    - [Ignoring the condition argument](#ignoring-the-condition-argument)
- [handler-case VS handler-bind](#handler-case-vs-handler-bind)
- [Handling conditions - handler-bind](#handling-conditions---handler-bind)
- [Creating conditions](#creating-conditions)
- [Signaling (throwing) conditions](#signaling-throwing-conditions)
- [Restarts, interactive choices in the debugger](#restarts-interactive-choices-in-the-debugger)
    - [Defining restarts](#defining-restarts)
    - [Calling restarts programmatically](#calling-restarts-programmatically)
    - [Using other restarts](#using-other-restarts)
    - [Prompting the user to enter a new value](#prompting-the-user-to-enter-a-new-value)
    - [Hide and show restarts](#hide-and-show-restarts)
- [Run some code, condition or not ("finally")](#run-some-code-condition-or-not-finally)
- [Resources](#resources)

<!-- markdown-toc end -->

## Ignore all errors (and return nil)

Sometimes you know that a function can fail and you just want to
ignore it: use `ignore-errors`:

~~~lisp
(ignore-errors
  (/ 3 0))
; in: IGNORE-ERRORS (/ 3 0)
;     (/ 3 0)
;
; caught STYLE-WARNING:
;   Lisp error during constant folding:
;   arithmetic error DIVISION-BY-ZERO signalled
;   Operation was (/ 3 0).
;
; compilation unit finished
;   caught 1 STYLE-WARNING condition
NIL
#<DIVISION-BY-ZERO {1008FF5F13}>
~~~

We get a welcome `division-by-zero` warning but the code runs well and
it returns two things: `nil` and the condition that was signaled. We
could not choose what to return.

Rembember that we can `inspect` the condition with a right click in Slime.


## Catching any condition - handler-case

<!-- we will say "handling" for handler-bind -->

`ignore-error` is built from `handler-case`. We can write the previous
example by catching the general `error` but now we can return whatever
we want:

~~~lisp
(handler-case (/ 3 0)
  (error (c)
    (format t "We caught a condition.~&")
    (values 0 c)))
; in: HANDLER-CASE (/ 3 0)
;     (/ 3 0)
;
; caught STYLE-WARNING:
;   Lisp error during constant folding:
;   Condition DIVISION-BY-ZERO was signalled.
;
; compilation unit finished
;   caught 1 STYLE-WARNING condition
We caught a condition.
0
#<DIVISION-BY-ZERO {1004846AE3}>
~~~

We also returned two values, 0 and the signaled condition.

The general form of `handler-case` is

~~~lisp
(handler-case (code that errors out)
   (condition-type (the-condition) ;; <-- optional argument
      (code))
   (another-condition (the-condition)
       ...))
~~~

We can also catch all conditions by matching `t`, like in a `cond`:

~~~lisp
(handler-case
    (progn
      (format t "This won't work…~%")
      (/ 3 0))
  (t (c)
    (format t "Got an exception: ~a~%" c)
    (values 0 c)))
;; …
;; This won't work…
;; Got an exception: arithmetic error DIVISION-BY-ZERO signalled
;; Operation was (/ 3 0).
;; 0
;; #<DIVISION-BY-ZERO {100608F0F3}>
~~~


## Catching a specific condition

We can specify what condition to handle:

~~~lisp
(handler-case (/ 3 0)
  (division-by-zero (c)
    (format t "Caught division by zero: ~a~%" c)))
;; …
;; Caught division by zero: arithmetic error DIVISION-BY-ZERO signalled
;; Operation was (/ 3 0).
;; NIL
~~~

This workflow is similar to a try/catch as found in other languages, but we can do more.


### Ignoring the condition argument

If you don't access the condition object in your handlers, but you still keep it has an argument for good practice, you'll see this compiler warning often:

```
; caught STYLE-WARNING:
;   The variable C is defined but never used.
```

To remove it, use a `declare` call as in:

~~~lisp
(handler-case (/ 3 0)
  (division-by-zero (c)
   (declare (ignore c))
   (format t "Caught division by zero~%"))) ;; we don't print "c" here and don't get the warning.
~~~

## handler-case VS handler-bind

`handler-case` is similar to the `try/catch` forms that we find in
other languages.

`handler-bind` (see the next examples), is what to use
when we need absolute control over what happens when a signal is
raised. It allows us to use the debugger and restarts, either
interactively or programmatically.

If some library doesn't catch all conditions and lets some bubble out
to us, we can see the restarts (established by `restart-case`)
anywhere deep in the stack, including restarts established by other
libraries whose this library called.  And *we can see the stack
trace*, with every frame that was called and, in some lisps, even see
local variables and such. Once we `handler-case`, we "forget" about
this, everything is unwound. `handler-bind` does *not* rewind the
stack.


## Handling conditions - handler-bind

Here we use `handler-bind`.

Its general form is:

~~~lisp
(handler-bind ((a-condition #'function-to-handle-it)
               (another-one #'another-function))
    (code that can...)
    (...error out))
~~~

So, our simple example:

~~~lisp
(handler-bind
             ((division-by-zero #'(lambda (c) (format t "hello condition~&"))))
           (/ 3 0))
~~~

This prints some warnings, then it prints our "hello" *and still
enters the debugger*. If we don't want to enter the debugger, we have
to define a restart and invoke it.


A real example with the
[`unix-opts`](https://github.com/mrkkrp/unix-opts) library, that
parses command line arguments. It defined some conditions:
`unknown-option`, `missing-arg` and `arg-parser-failed`, and it is up
to use to write what to do in these cases.

~~~lisp
(handler-bind ((opts:unknown-option #'unknown-option)
               (opts:missing-arg #'missing-arg)
               (opts:arg-parser-failed #'arg-parser-failed))
  (opts:get-opts))
~~~

Our `unknown-option` function is simple and looks like this:

~~~lisp
(defun unknown-option (condition)
  (format t "~s option is unknown.~%" (opts:option condition))
  (opts:describe)
  (exit)) ;; <-- we return to the command line, no debugger.
~~~

it takes the condition as parameter, so we can read information from
it if needed. Here we get the name of the erronous option with the
defined reader `(opts:option condition)` (see below).


## Creating conditions

With `define-condition`, and we can inherit from `error` or `simple-error`:

<!-- todo: differences ? -->

<!-- make-condition ? -->

~~~lisp
(define-condition my-division-by-zero (error) ())
~~~

It is a regular class, so we can add information into slots. Here, we
add a custom message:

~~~lisp
(define-condition my-division-by-zero (error)
  ((dividend :initarg :dividend
            :reader dividend)) ;; <= so we'll get the dividend with (dividend condition), as soon as on the next line.
  ;; the :report is the message into the debugger:
  (:report (lambda (condition stream) (format stream "You were going to divide ~a by zero.~&" (dividend condition)))))
~~~

The general form looks like a regular class definition:

~~~lisp
(define-condition my-condition (condition-it-inherits-from)
  ;; list of arguments, can be "()".
  ((message :initarg :message
            :reader my-condition-message)
   (second ...))
  ;; class arguments
  (:report (lambda (condition stream) (...))) ;; what is printed in the REPL.
  (:documentation "a string")) ;; good practice ;)
~~~

Now when we throw this condition we must pass it an error message (it
is a required argument), and read it with `error-message` (the
`:reader`).

What's in `:report` will be printed in the REPL.

Let's try our condition. We define a simple function that checks our
divisor, and signals our condition if it is equal to zero:

~~~lisp
(defun my-division (x y)
    (if (= y 0)
        (error 'MY-DIVISION-BY-ZERO :dividend x))
    (/ x y))
~~~

When we use it, we enter the debugger:

~~~lisp
(my-division 3 0)
;;
;; into the debugger:
;;
You were going to divide 3 by zero.
   [Condition of type MY-DIVISION-BY-ZERO]

Restarts:
 0: [RETRY] Retry SLIME REPL evaluation request.
 1: [*ABORT] Return to SLIME's top level.
 2: [ABORT] abort thread (#<THREAD "repl-thread" RUNNING {1002957FA3}>)

Backtrace:
  0: (MY-DIVISION 3 0)
~~~

We can inspect the backtrace, go to the source (`v` in Slime), etc.

---

Here is how `unix-opts` defines its `unknown-option` condition:

~~~lisp
(define-condition troublesome-option (simple-error)
  ((option
    :initarg :option
    :reader option))
  (:report (lambda (c s) (format s "troublesome option: ~s" (option c))))
  (:documentation "Generalization over conditions that have to do with some
particular option."))

(define-condition unknown-option (troublesome-option)
  ()
  (:report (lambda (c s) (format s "unknown option: ~s" (option c))))
  (:documentation "This condition is thrown when parser encounters
unknown (not previously defined with `define-opts') option."))
~~~


## Signaling (throwing) conditions

We can use `error`, like we did above. Two ways:

- `(error "some text")`: signals a condition of type `simple-error`
- `(error 'my-error :message "We did this and this and it didn't work.")`

Throwing these conditions will enter the interactive debugger,
where a few options will be presented by default. We can give
more options with *restarts*,
and we can prevent from entering the debugger by handling the condition and invoking a restart.

Simple example from `unix-opts`: it adds information into the `option` slot:

~~~lisp
(error 'unknown-option
        :option opt)
~~~


## Restarts, interactive choices in the debugger

### Defining restarts

Restarts are the choices we get in the debugger, which always has the
`RETRY` and `ABORT` ones. We can add choices to the top of the list:

~~~lisp
(defun division-restarter ()
  (restart-case (/ 3 0)
    (return-zero () 0)
    (divide-by-one () (/ 3 1))))
~~~

By calling this stupid function we get two new choices at the top of the debugger:

![](simple-restarts.png)

Note: read in lisper.in's blogpost on csv parsing (see Resources) how
this system was used effectively in production.

But that's not all, by handling restarts we can start over the
operation as if the error didn't occur (as seen in the stack).

### Calling restarts programmatically

With `invoke-restart`.

~~~lisp
(defun division-and-bind ()
           (handler-bind
               ((error (lambda (c)
                         (format t "Got error: ~a~%" c) ;; error-message
                         (format t "and will divide by 1~&")
                         (invoke-restart 'divide-by-one))))
             (division-restarter)))
;; (DIVISION-AND-BIND)
;; Got error: arithmetic error DIVISION-BY-ZERO signalled
;; and will divide by 1
;; Operation was (/ 3 0).
;; 3
~~~

Note that we called the form that contains our restarts
(`division-restarter`) and not the function that throws the error.

### Using other restarts

`find-restart 'name-of-restart` will return the most recent bound
restart with the given name, or `nil`. We can invoke it with
`invoke-restart`.

### Prompting the user to enter a new value

Let's add a restart in our `division-restarter` to offer the user to
enter a new dividend, and run the division again.

~~~lisp
(defun division-restarter ()
  (restart-case (/ 3 0)
    (return-nil () nil)
    (divide-by-one () (/ 3 1))
    (choose-another-dividend (new-dividend)
      :report "Please choose another dividend"
      :interactive (lambda ()
                     (format t "Enter a new dividend: ")
                     (list (read))) ;; <-- must return a list.
      (format t "New division: 3/~a = ~a~&" new-dividend (/ 3 new-dividend)))))
~~~

We get prompted in the debugger:

```
arithmetic error DIVISION-BY-ZERO signalled
Operation was (/ 3 0).
  [Condition of type DIVISION-BY-ZERO]

Restarts:
 0: [RETURN-NIL] RETURN-NIL
 1: [DIVIDE-BY-ONE] DIVIDE-BY-ONE
 2: [CHOOSE-ANOTHER-DIVIDEND] Please choose another dividend <-- new
 3: [RETRY] Retry SLIME REPL evaluation request.
 4: [*ABORT] Return to SLIME's top level.
 5: [ABORT] abort thread (#<THREAD "repl-thread" RUNNING {1002A47FA3}>)
```

The new `choose-another-dividend` restart takes an argument for the
new dividend, that will be fed by the `:interactive` lambda, which
`read`s for user input and must return a list.

We use it like this:

~~~lisp
(division-restarter)
;;
;; Entered debugger, chose the 2nd restart.
;;
Enter a new dividend: 10  <-- got prompted to enter a new value.
New division: 3/10 = 3/10
NIL
~~~

In a real situation we might want to call our "restarter" recursively,
to get into the debugger again if we enter a bad value.

### Hide and show restarts

Restarts can be hidden. In `restart-case`, in addition to `:report`
and `:interactive`, they also accept a `:test` key:

~~~lisp
(restart-case
   (return-zero ()
     :test (lambda ()
             (some-test))
    ...
~~~

## Run some code, condition or not ("finally")

The "finally" part of others `try/catch/finally` forms is done with `unwind-protect`.

It is the construct used in "with-" macros, like `with-open-file`,
which always closes the file after it.


You're now more than ready to write some code and to dive into other resources !


## Resources

* [Practical Common Lisp: "Beyond Exception Handling: Conditions and Restarts"](http://gigamonkeys.com/book/beyond-exception-handling-conditions-and-restarts.html) - the go-to tutorial, more explanations and primitives.
* Common Lisp Recipes, chap. 12, by E. Weitz
* [language reference](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node317.html)
* [lisper.in](https://lisper.in/restarts#signaling-validation-errors) - example with parsing a csv file and using restarts with success, [in a flight travel company](https://www.reddit.com/r/lisp/comments/7k85sf/a_tutorial_on_conditions_and_restarts/drceozm/).
* [Condition Handling in the Lisp family of languages](http://www.nhplace.com/kent/Papers/Condition-Handling-2001.html)
* [z0ltan.wordpress.com](https://z0ltan.wordpress.com/2016/08/06/conditions-and-restarts-in-common-lisp/)
* [https://github.com/svetlyak40wt/python-cl-conditions](https://github.com/svetlyak40wt/python-cl-conditions) - implementation of the CL conditions system in Python.
