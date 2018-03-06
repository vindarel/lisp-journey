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
(let ((*standard-output* (make-string-output-stream)))
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

- [make-string-output-stream on the hyperspec](http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_s_2.htm#make-string-output-stream)
