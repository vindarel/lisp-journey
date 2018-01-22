+++
date = "2018-01-22T07:51:49+01:00"
title = "Print licences used by a given project and its dependencies"
draft = false
+++

[print-licenses](https://github.com/vindarel/print-licenses/) is a little utility found in
[Steve Losh's gigantic utilities](https://github.com/sjl/cl-losh/blob/master/losh.lisp)
and ported to a stand alone project.

Example usage:

~~~lisp
  (print-licenses 'fast-io)
  =>
  alexandria           | Public Domain / 0-clause MIT
  babel                | MIT
  cffi                 | MIT
  cffi-grovel          | MIT
  cffi-toolchain       | MIT
  fast-io              | NewBSD
  static-vectors       | MIT
  trivial-features     | MIT
  trivial-gray-streams | MIT
  uiop                 | Unspecified
~~~

It may be available on february, 2018 Quicklisp update ([request](https://github.com/quicklisp/quicklisp-projects/issues/1432)).

One potential source of caution ([feedback on reddit](https://www.reddit.com/r/Common_Lisp/comments/7qca2v/print_licenses_used_by_the_given_project_and_its/)):

> what many authors put as the license in their asd file is not the license file that is actually included in the source code.

---

Let's read the source, there are many useful bits. The core of the job is:

~~~lisp
(defun print-licenses (quicklisp-project-designator)
  (print-table (sort (license-list quicklisp-project-designator)
                     #'string<
                     :key #'car)))

(defun license-list (quicklisp-project-designator)
  (remove-duplicates
    (mapcar (alexandria:rcurry #'coerce 'list)
            (alexandria:flatten (license-tree quicklisp-project-designator)))
    :key #'car :test #'string=))

(defun license-tree (quicklisp-project-designator)
  (let ((sys (ql-dist:dependency-tree quicklisp-project-designator)))
    (assert (not (null sys)) ()
      "Cannot find Quicklisp project for designator ~S"
      quicklisp-project-designator)
    (shut-up
      (ql:quickload quicklisp-project-designator))
    (map-tree
      (lambda (s)
        (vector (slot-value s 'ql-dist:name)
                (or (asdf:system-license
                      (asdf:find-system
                        (ql-dist:system-file-name s)))
                    "Unspecified")))
      sys)))

~~~

and those are the remaining building blocks, with a useful
`print-table` function, and three of them taken from
[Quickutil](http://quickutil.org/). See their website and how sjl does
to include them (and only them, to keep lightweight dependencies) in a
project without copy-pasting.

~~~lisp
(defmacro shut-up (&body body)
  "Run `body` with stdout and stderr redirected to the void."
  `(let ((*standard-output* (make-broadcast-stream))
         (*error-output* (make-broadcast-stream)))
     ,@body))

;; from Quickutil.
(defun map-tree (function tree)
    "Map `function` to each of the leave of `tree`."
    (check-type tree cons)
    (labels ((rec (tree)
               (cond
                 ((null tree) nil)
                 ((atom tree) (funcall function tree))
                 ((consp tree)
                  (cons (rec (car tree))
                        (rec (cdr tree)))))))
      (rec tree)))

;; from Quickutil
(defun aesthetic-string (thing)
  "Return the string used to represent `thing` when printing aesthetically."
  (format nil "~A" thing))

;; from Quickutil
(defun weave (&rest lists)
  "Return a list whose elements alternate between each of the lists
`lists`. Weaving stops when any of the lists has been exhausted."
  (apply #'mapcan #'list lists))

(defun print-table (rows)
  "Print `rows` as a nicely-formatted table.
  Each row should have the same number of colums.
  Columns will be justified properly to fit the longest item in each one.
  Example:
    (print-table '((1 :red something)
                   (2 :green more)))
    =>
    1 | RED   | SOMETHING
    2 | GREEN | MORE
  "
  (when rows
    (iterate
      (with column-sizes =
            (reduce (curry #'mapcar #'max)
                    (mapcar (curry #'mapcar (compose #'length #'aesthetic-string))
                            rows))) ; lol
      (for row :in rows)
      (format t "~{~vA~^ | ~}~%" (weave column-sizes row))))
  (values))
~~~
