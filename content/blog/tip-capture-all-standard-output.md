+++
date = "2018-03-06T07:51:49+01:00"
title = "Tip: capture standard and error output"
draft = false
+++


What if we want to capture standard (and/or error) output in order to
ignore it or post-process it ? It's very simple, a
[little search](https://stackoverflow.com/questions/35333715/lisp-capture-stdout-and-stderr-store-it-in-separate-variables)
and we're good:

~~~lisp
(let ((*standard-output* (make-string-output-stream))
      (*error-output* (make-string-output-stream)))
  (apply function args) ;; anything
  (setf standard-output (get-output-stream-string *standard-output*)))
(print-results standard-output))
~~~

and now in `print-results` we can print to standard output without
being intercepted (and in our case, we'll highlight some user-defined
keywords).

Above, just don't forget to get the output content with
`(get-output-stream-string *standard-output*)`.

A thing to note is that if your app printed stuff on error output and
standard output consecutively, now it will print all standard output
as a single block.

(**edit**) Of course, `with-output-to-string` is simpler to capture one stream:

~~~lisp
(setf standard-output (with-output-to-string (*standard-output*)
                        (apply function args)))
~~~

**edit 2**, thanks to [redditers](https://www.reddit.com/r/learnlisp/comments/837i0j/tip_capture_standard_and_error_output/):

Don't bind *`standard-output`* directly; bind the string stream to a
lexical, then bind `*standard-output*` to that:

~~~lisp
(with-output-to-string (s)
  (let ((*standard-output* s)) (write-string "abc")))
-> "abc"
~~~

Now, let's bind both `*standard-output*` and `*standard-error*` to s:

~~~lisp
(with-output-to-string (s)
  (let ((*standard-output* s)
        (*standard-error* s))
    (write-string "abc")
    (write-string "def" *standard-error*)))
-> "abcdef"
~~~

Eliminate s and just bind `*standard-output*` and then tie
`*standard-error*` to the same stream:

~~~lisp
(with-output-to-string (*standard-output*)
  (let ((*standard-error* *standard-output*))
    (write-string "abc")
    (write-string "def" *standard-error*)))
--> "abcdef"
~~~


The conclusion stays: it's handy and easy :)
