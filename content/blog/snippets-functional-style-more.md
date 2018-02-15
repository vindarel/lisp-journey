+++
date = "2018-01-23T07:51:49+01:00"
title = "snippets - functional style, sequences, debugging and more utilities"
draft = false
+++

From
[sjl's utilities](https://github.com/sjl/cl-losh/blob/master/losh.lisp)
(thanks so much for the nice docstrings). The goal here is to read
some code and learn about (hidden) gems.

The following snippets should be copy-pastable. They are the ones I
find most interesting, I left some behind.

To reduce the dependency load, Alexandria or Quickutil functions can
be imported one by one with [Quickutil](http://quickutil.org/).

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [Higher order functions](#higher-order-functions)
- [Sequences](#sequences)
- [Debugging and logging](#debugging-and-logging)
- [Profiling (with SBCL)](#profiling-with-sbcl)

<!-- markdown-toc end -->


## Higher order functions

See also https://github.com/mikelevins/folio2 and [How to do functional programming in CL](/blog/functional-programming-in-common-lisp/).


~~~lisp
(defun juxt (&rest functions)
  "Return a function that will juxtapose the results of `functions`.
  This is like Clojure's `juxt`.  Given functions `(f0 f1 ... fn)`, this will
  return a new function which, when called with some arguments, will return
  `(list (f0 ...args...) (f1 ...args...) ... (fn ...args...))`.
  Example:
    (funcall (juxt #'list #'+ #'- #'*) 1 2)
    => ((1 2) 3 -1 2)
  "
  (lambda (&rest args)
    (mapcar (alexandria:rcurry #'apply args) functions)))
~~~


~~~lisp
(defun nullary (function &optional result)
  "Return a new function that acts as a nullary-patched version of `function`.
  The new function will return `result` when called with zero arguments, and
  delegate to `function` otherwise.
  Examples:
    (max 1 10 2) ; => 10
    (max)        ; => invalid number of arguments
    (funcall (nullary #'max))          ; => nil
    (funcall (nullary #'max 0))        ; => 0
    (funcall (nullary #'max 0) 1 10 2) ; => 10
    (reduce #'max nil)                  ; => invalid number of arguments
    (reduce (nullary #'max) nil)        ; => nil
    (reduce (nullary #'max :empty) nil) ; => :empty
    (reduce (nullary #'max) '(1 10 2))  ; => 10
  "
  (lambda (&rest args)
    (if (null args) result (apply function args))))
~~~


~~~lisp
(defmacro gathering (&body body)
  ;; https://github.com/sjl/cl-losh/blob/master/losh.lisp#L515
  "Run `body` to gather some things and return a fresh list of them.
  `body` will be executed with the symbol `gather` bound to a function of one
  argument.  Once `body` has finished, a list of everything `gather` was called
  on will be returned.
  It's handy for pulling results out of code that executes procedurally and
  doesn't return anything, like `maphash` or Alexandria's `map-permutations`.
  The `gather` function can be passed to other functions, but should not be
  retained once the `gathering` form has returned (it would be useless to do so
  anyway).
  Examples:
    (gathering
      (dotimes (i 5)
        (gather i))
    =>
    (0 1 2 3 4)
    (gathering
      (mapc #'gather '(1 2 3))
      (mapc #'gather '(a b)))
    =>
    (1 2 3 a b)
  "
  (with-gensyms (result)
    `(let ((,result (make-queue)))
      (flet ((gather (item)
               (enqueue item ,result)))
        (declare (dynamic-extent #'gather))
        ,@body)
      (queue-contents ,result))))
~~~

Here we need the `queue` struct.

~~~lisp
(defstruct (queue (:constructor make-queue))
  (contents nil :type list)
  (last nil :type list)
  (size 0 :type fixnum))

;; real code is richer, with inline and inlinable function declarations.

(defun make-queue ()
  "Allocate and return a fresh queue."
  (make-queue%))

(defun queue-empty-p (queue)
  "Return whether `queue` is empty."
  (zerop (queue-size queue)))

(defun enqueue (item queue)
  "Enqueue `item` in `queue`, returning the new size of the queue."
  (let ((cell (cons item nil)))
    (if (queue-empty-p queue)
      (setf (queue-contents queue) cell)
      (setf (cdr (queue-last queue)) cell))
    (setf (queue-last queue) cell))
  (incf (queue-size queue)))

(defun dequeue (queue)
  "Dequeue an item from `queue` and return it."
  (when (zerop (decf (queue-size queue)))
    (setf (queue-last queue) nil))
  (pop (queue-contents queue)))

(defun queue-append (queue list)
  "Enqueue each element of `list` in `queue` and return the queue's final size."
  (loop :for item :in list
        :for size = (enqueue item queue)
        :finally (return size)))
~~~

## Sequences

~~~lisp
(defun frequencies (sequence &key (test 'eql))
  ;; https://github.com/sjl/cl-losh/blob/master/losh.lisp#L1910
  "Return a hash table containing the frequencies of the items in `sequence`.
  Uses `test` for the `:test` of the hash table.
  Example:
    (frequencies '(foo foo bar))
    => {foo 2
        bar 1}
  "
  (iterate
    (with result = (make-hash-table :test test))
    (for i :in-whatever sequence)
    (incf (gethash i result 0))
    (finally (return result))))
~~~

~~~lisp
(defun proportions (sequence &key (test 'eql) (float t))
  "Return a hash table containing the proportions of the items in `sequence`.
  Uses `test` for the `:test` of the hash table.
  If `float` is `t` the hash table values will be coerced to floats, otherwise
  they will be left as rationals.
  Example:
    (proportions '(foo foo bar))
    => {foo 0.66666
        bar 0.33333}
    (proportions '(foo foo bar) :float nil)
    => {foo 2/3
        bar 1/3}
  "
  (let* ((freqs (frequencies sequence :test test))
         (total (reduce #'+ (hash-table-values freqs)
                        :initial-value (if float 1.0 1))))
    (mutate-hash-values (lambda (v) (/ v total))
                        freqs)))
~~~


~~~lisp
(defun group-by (function sequence &key (test #'eql) (key #'identity))
  "Return a hash table of the elements of `sequence` grouped by `function`.
  This function groups the elements of `sequence` into buckets.  The bucket for
  an element is determined by calling `function` on it.
  The result is a hash table (with test `test`) whose keys are the bucket
  identifiers and whose values are lists of the elements in each bucket.  The
  order of these lists is unspecified.
  If `key` is given it will be called on each element before passing it to
  `function` to produce the bucket identifier.  This does not effect what is
  stored in the lists.
  Examples:
    (defparameter *items* '((1 foo) (1 bar) (2 cats) (3 cats)))
    (group-by #'first *items*)
    ; => { 1 ((1 foo) (1 bar))
    ;      2 ((2 cats))
    ;      3 ((3 cats)) }
    (group-by #'second *items*)
    ; => { foo  ((1 foo))
    ;      bar  ((1 bar))
    ;      cats ((2 cats) (3 cats)) }
    (group-by #'evenp *items* :key #'first)
    ; => { t   ((2 cats))
    ;      nil ((1 foo) (1 bar) (3 cats)) }
  "
  (iterate
    (with result = (make-hash-table :test test))
    (for i :in-whatever sequence)
    (push i (gethash (funcall function (funcall key i)) result))
    (finally (return result))))


(defun take-list (n list)
  (iterate (declare (iterate:declare-variables))
           (repeat n)
           (for item :in list)
           (collect item)))

(defun take-seq (n seq)
  (subseq seq 0 (min n (length seq))))
~~~

~~~lisp
(defmacro do-repeat (n &body body)
  "Perform `body` `n` times."
  `(dotimes (,(gensym) ,n)
     ,@body))
~~~

~~~lisp
(defmacro do-range (ranges &body body)
  "Perform `body` on the given `ranges`.

  Each range in `ranges` should be of the form `(variable from below)`.  During
  iteration `body` will be executed with `variable` bound to successive values
  in the range [`from`, `below`).

  If multiple ranges are given they will be iterated in a nested fashion.

  Example:

    (do-range ((x  0  3)
               (y 10 12))
      (pr x y))
    ; =>
    ; 0 10
    ; 0 11
    ; 1 10
    ; 1 11
    ; 2 10
    ; 2 11

  "
  (if (null ranges)
    `(progn ,@body)
    (destructuring-bind (var from below) (first ranges)
      `(loop :for ,var :from ,from :below ,below
             :do (do-range ,(rest ranges) ,@body)))))
~~~


~~~lisp
(defun enumerate (sequence &key (start 0) (step 1) key)
  "Return an alist of `(n . element)` for each element of `sequence`.
  `start` and `step` control the values generated for `n`, NOT which elements of
  the sequence are enumerated.
  Examples:
    (enumerate '(a b c))
    ; => ((0 . A) (1 . B) (2 . C))
    (enumerate '(a b c) :start 1)
    ; => ((1 . A) (2 . B) (3 . C))
    (enumerate '(a b c) :key #'ensure-keyword)
    ; => ((0 . :A) (1 . :B) (2 . :C))
  "
  (iterate (for el :in-whatever sequence)
           (for n :from start :by step)
           (collect (cons n (if key
                              (funcall key el)
                              el)))))
~~~

uses `iterate`, on Quicklisp (see also Shinmera's [For](https://github.com/Shinmera/for)).

The following`take` is taken from [Serapeum](https://github.com/TBRSS/serapeum/) (also available in CL21).

The original helpers (take-list, etc) are originally inlined for optimal performance
with a custom "defun-inline".


~~~lisp
(defun take (n seq)
  "Return a fresh sequence of the first `n` elements of `seq`.
  The result will be of the same type as `seq`.
  If `seq` is shorter than `n` a shorter result will be returned.
  Example:
    (take 2 '(a b c))
    => (a b)
    (take 4 #(1))
    => #(1)
  From Serapeum.
  "
  (check-type n array-index)
  (ctypecase seq
    (list (take-list n seq))
    (sequence (take-seq n seq))))

(defun take-list (n list)
  (iterate (declare (iterate:declare-variables))
           (repeat n)
           (for item :in list)
           (collect item)))

(defun take-seq (n seq)
  (subseq seq 0 (min n (length seq))))
~~~

~~~lisp
(defun take-while-list (predicate list)
  (iterate (for item :in list)
           (while (funcall predicate item))
           (collect item)))

(defun take-while-seq (predicate seq)
  (subseq seq 0 (position-if-not predicate seq)))

(defun take-while (predicate seq)
  "Take elements from `seq` as long as `predicate` remains true.
  The result will be a fresh sequence of the same type as `seq`.
  Example:
    (take-while #'evenp '(2 4 5 6 7 8))
    ; => (2 4)
    (take-while #'evenp #(1))
    ; => #()
  "
  (ctypecase seq
    (list (take-while-list predicate seq))
    (sequence (take-while-seq predicate seq))))
~~~


~~~lisp
(defun drop-list (n list)
  (copy-list (nthcdr n list)))

(defun drop-seq (n seq)
  (subseq seq (min n (length seq))))

(defun drop (n seq)
  "Return a fresh copy of the `seq` without the first `n` elements.
  The result will be of the same type as `seq`.
  If `seq` is shorter than `n` an empty sequence will be returned.
  Example:
    (drop 2 '(a b c))
    => (c)
    (drop 4 #(1))
    => #()
  From Serapeum.
  "
  (check-type n array-index)
  (ctypecase seq
    (list (drop-list n seq))
    (sequence (drop-seq n seq))))
~~~


~~~lisp
(defun drop-while-list (predicate list)
  (iterate (for tail :on list)
           (while (funcall predicate (first tail)))
           (finally (return (copy-list tail)))))

(defun drop-while-seq (predicate seq)
  (let ((start (position-if-not predicate seq)))
    (if start
      (subseq seq start)
      (subseq seq 0 0))))

(defun drop-while (predicate seq)
  "Drop elements from `seq` as long as `predicate` remains true.
  The result will be a fresh sequence of the same type as `seq`.
  Example:
    (drop-while #'evenp '(2 4 5 6 7 8))
    ; => (5 6 7 8)
    (drop-while #'evenp #(2))
    ; => #(2)
  "
  (ctypecase seq
    (list (drop-while-list predicate seq))
    (sequence (drop-while-seq predicate seq))))
~~~

~~~lisp
(defun extrema (predicate sequence)
  "Return the smallest and largest elements of `sequence` according to `predicate`.
  `predicate` should be a strict ordering predicate (e.g. `<`).
  Returns the smallest and largest elements in the sequence as two values,
  respectively.
  "
  (iterate (with min = (elt sequence 0))
           (with max = (elt sequence 0))
           (for el :in-whatever sequence)
           (when (funcall predicate el min) (setf min el))
           (when (funcall predicate max el) (setf max el))
           (finally (return (values min max)))))
~~~


~~~lisp
(defun summation (sequence &key key)
  "Return the sum of all elements of `sequence`.
  If `key` is given, it will be called on each element to compute the addend.
  This function's ugly name was chosen so it wouldn't clash with iterate's `sum`
  symbol.  Sorry.
  Examples:
    (sum #(1 2 3))
    ; => 6
    (sum '(\"1\" \"2\" \"3\") :key #'parse-integer)
    ; => 6
    (sum '(\"1\" \"2\" \"3\") :key #'length)
    ; => 3
  "
  (if key
    (iterate (for n :in-whatever sequence)
             (sum (funcall key n)))
    (iterate (for n :in-whatever sequence)
             (sum n))))
~~~


~~~lisp
(defun product (sequence &key key)
  ;; https://github.com/sjl/cl-losh/blob/master/losh.lisp#L2181
  "Return the product of all elements of `sequence`.
  If `key` is given, it will be called on each element to compute the
  multiplicand.
  Examples:
    (product #(1 2 3))
    ; => 6
    (product '(\"1\" \"2\" \"3\") :key #'parse-integer)
    ; => 6
    (product '(\"1\" \"2\" \"3\") :key #'length)
    ; => 1
  "
  (if key
    (iterate (for n :in-whatever sequence)
             (multiplying (funcall key n)))
    (iterate (for n :in-whatever sequence)
             (multiplying n))))
~~~

## Debugging and logging


~~~lisp
(defun pr (&rest args)
  "Print `args` readably, separated by spaces and followed by a newline.
  Returns the first argument, so you can just wrap it around a form without
  interfering with the rest of the program.
  This is what `print` should have been.
  "
  (format t "~{~S~^ ~}~%" args)
  (finish-output)
  (first args))
~~~

~~~lisp
(defmacro prl (&rest args)
  "Print `args` labeled and readably.
  Each argument form will be printed, then evaluated and the result printed.
  One final newline will be printed after everything.
  Returns the last result.
  Examples:
    (let ((i 1)
          (l (list 1 2 3)))
      (prl i (second l)))
    ; =>
    i 1
    (second l) 2
  "
  `(prog1
    (progn ,@(mapcar (lambda (arg) `(pr ',arg ,arg)) args))
    (terpri)
    (finish-output)))
~~~


~~~lisp
(defmacro shut-up (&body body)
  "Run `body` with stdout and stderr redirected to the void."
  `(let ((*standard-output* (make-broadcast-stream))
         (*error-output* (make-broadcast-stream)))
    ,@body))
~~~


~~~lisp
(defmacro comment (&body body)
  "Do nothing with a bunch of forms.
  Handy for block-commenting multiple expressions.
  "
  (declare (ignore body))
  nil)
~~~

Pretty-print a table.

Didn't test.

See also https://github.com/vindarel/cl-ansi-term


<!-- TODO: test -->

~~~lisp
(defun print-table (rows)
  ;; https://github.com/sjl/cl-losh/blob/master/losh.lisp#L2334
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
            (reduce (alexandria:curry #'mapcar #'max)
                    (mapcar (alexandria:curry #'mapcar (compose #'length #'aesthetic-string))
                            rows))) ; lol
      (for row :in rows)
      (format t "~{~vA~^ | ~}~%" (weave column-sizes row))))
  (values))


;; from Quickutil.
(defun ensure-function (function-designator)
    "Returns the function designated by `function-designator`:
if `function-designator` is a function, it is returned, otherwise
it must be a function name and its `fdefinition` is returned."
    (if (functionp function-designator)
        function-designator
        (fdefinition function-designator)))

;; from Quickutil.
(defun compose (function &rest more-functions)
    "Returns a function composed of `function` and `more-functions` that applies its ;
arguments to to each in turn, starting from the rightmost of `more-functions`,
and then calling the next one with the primary value of the last."
    (declare (optimize (speed 3) (safety 1) (debug 1)))
    (reduce (lambda (f g)
              (let ((f (ensure-function f))
                    (g (ensure-function g)))
                (lambda (&rest arguments)
                  (declare (dynamic-extent arguments))
                  (funcall f (apply g arguments)))))
            more-functions
            :initial-value function))

(defun make-gensym-list (length &optional (x "G"))
    "Returns a list of `length` gensyms, each generated as if with a call to `make-gensym`,
using the second (optional, defaulting to `\"G\"`) argument."
    (let ((g (if (typep x '(integer 0)) x (string x))))
      (loop repeat length
            collect (gensym g))))

  (define-compiler-macro compose (function &rest more-functions)
    (labels ((compose-1 (funs)
               (if (cdr funs)
                   `(funcall ,(car funs) ,(compose-1 (cdr funs)))
                   `(apply ,(car funs) arguments))))
      (let* ((args (cons function more-functions))
             (funs (make-gensym-list (length args) "COMPOSE")))
        `(let ,(loop for f in funs for arg in args
                     collect `(,f (ensure-function ,arg)))
           (declare (optimize (speed 3) (safety 1) (debug 1)))
           (lambda (&rest arguments)
             (declare (dynamic-extent arguments))
             ,(compose-1 funs))))))


;; from Quickutil.
(defun weave (&rest lists)
    "Return a list whose elements alternate between each of the lists
`lists`. Weaving stops when any of the lists has been exhausted."
    (apply #'mapcan #'list lists))

(defun aesthetic-string (thing)
  "Return the string used to represent `thing` when printing aesthetically."
  (format nil "~A" thing))

~~~


Pretty print a hash-table:

~~~lisp
(defun print-hash-table (hash-table &optional (stream t))
  "Print a pretty representation of `hash-table` to `stream.`
  Respects `*print-length*` when printing the elements.
  "
  (let* ((keys (alexandria:hash-table-keys hash-table))
         (vals (alexandria:hash-table-values hash-table))
         (count (hash-table-count hash-table))
         (key-width (-<> keys
                      (mapcar (alexandria:compose #'length #'prin1-to-string) <>)
                      (reduce #'max <> :initial-value 0)
                      (clamp 0 20 <>))))
    (print-unreadable-object (hash-table stream :type t)
      (princ
        ;; Something shits the bed and output gets jumbled (in SBCL at least) if
        ;; we try to print to `stream` directly in the format statement inside
        ;; `print-unreadable-object`, so instead we can just render to a string
        ;; and `princ` that.
        (format nil ":test ~A :count ~D {~%~{~{  ~vs ~s~}~%~}}"
                (hash-table-test hash-table)
                count
                (loop
                  :with limit = (or *print-length* 40)
                  :for key :in keys
                  :for val :in vals
                  :for i :from 0 :to limit
                  :collect
                  (if (= i limit)
                    (list key-width :too-many-items (list (- count i) :more))
                    (list key-width key val))))
        stream)))
  (terpri stream)
  (values))

(defun pht (hash-table &optional (stream t))
  "Synonym for `print-hash-table` for less typing at the REPL."
  (print-hash-table hash-table stream))

(defun print-hash-table-concisely (hash-table &optional (stream t))
  "Print a concise representation of `hash-table` to `stream.`
  Should respect `*print-length*` when printing the elements.
  "
  (print-unreadable-object (hash-table stream :type t)
    (prin1 (hash-table-test hash-table))
    (write-char #\space stream)
    (prin1 (hash-table-contents hash-table) stream)))

;; needed:
(defun clamp (from to value)
  "Clamp `value` between `from` and `to`."
  (let ((max (max from to))
        (min (min from to)))
    (cond
      ((> value max) max)
      ((< value min) min)
      (t value))))

;; see
(defmacro -<> (expr &rest forms)
  "Thread the given forms, with `<>` as a placeholder."
  ;; I am going to lose my fucking mind if I have to program lisp without
  ;; a threading macro, but I don't want to add another dep to this library, so
  ;; here we are.
  `(let* ((<> ,expr)
          ,@(mapcar (lambda (form)
                      (if (symbolp form)
                        `(<> (,form <>))
                        `(<> ,form)))
                    forms))
     <>))

~~~

For the `-<>` threading macro, see [cl-arrows](https://github.com/nightfly19/cl-arrows) and [arrow-macros](https://github.com/hipeta/arrow-macros).

## Profiling (with SBCL)

~~~lisp
#+sbcl
(defun dump-profile (filename)
  (with-open-file (*standard-output* filename
                                     :direction :output
                                     :if-exists :supersede)
    (sb-sprof:report :type :graph
                     :sort-by :cumulative-samples
                     :sort-order :ascending)
    (sb-sprof:report :type :flat
                     :min-percent 0.5)))

#+sbcl
(defun start-profiling (&key call-count-packages (mode :cpu))
  "Start profiling performance.  SBCL only.
  `call-count-packages` should be a list of package designators.  Functions in
  these packages will have their call counts recorded via
  `sb-sprof::profile-call-counts`.
  "
  (sb-sprof::reset)
  (-<> call-count-packages
    (mapcar #'mkstr <>)
    (mapcar #'string-upcase <>)
    (mapc #'sb-sprof::profile-call-counts <>))
  (sb-sprof::start-profiling :max-samples 50000
                             :mode mode
                             ; :mode :time
                             :sample-interval 0.01
                             :threads :all))

#+sbcl
(defun stop-profiling (&optional (filename "lisp.prof"))
  "Stop profiling performance and dump a report to `filename`.  SBCL only."
  (sb-sprof::stop-profiling)
  (dump-profile filename))

#+sbcl
(defmacro profile (&body body)
  "Profile `body` and dump the report to `lisp.prof`."
  `(progn
     (start-profiling)
     (unwind-protect
         (time (progn ,@body))
       (stop-profiling))))
~~~
