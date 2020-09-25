---
title: "String manipulation is frustrating [fixed]"
date: 2017-05-02T11:07:01+02:00
draft: false
tags: ["libraries",]
---

One of the first things I wanted to do in the REPL was some string
manipulation. But it was tedious.

To trim whitespace, and I mean all whitespaces, we had to define
`#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return
#\Rubout`.

To concatenate two strings: either giving an unusual `'string` argument to
`concatenate`, like this:

~~~lisp
(concatenate 'string "fo" "o")
~~~

either we had to use a `format` construct, which is another source of
frustration for (impatient) beginners, and sure isn't straightforward
and self-explanatory.

Many common stuff was split in various external libraries
(`cl-ppcre`), and many common stuff was made more difficult than
necessary (weird format construct again, entering a regexp, thus
esaping what's necessary, when all you want to do is simple search and
replace, dealing with strings' lengths and corner cases, lack of
verbs,… see below).

And all of that with many inconsistencies (the string as first
argument, then as the last, etc).

So I just joined everything in a little library, which has now more
features. Let's see its code and its tests to learn at the canonical
way to do stuff, their shortcomings, and the library api at the same
time.

I just don't know how come this lib didn't exist yet.

## `str`

You can install it with

    (ql:quickload "str")

See on https://github.com/vindarel/cl-str.

### Package definition

~~~lisp
(in-package #:asdf-user)

(defsystem :str
  :source-control (:git "git@github.com:vindarel/cl-s.git")
  :description "Modern, consistent and terse Common Lisp string manipulation library."
  :depends-on (:prove :cl-ppcre)  ;; <= depends only on cl-ppcre.
  :components ((:file "str"))
  )
~~~


### Trim

~~~lisp
(defvar *whitespaces* '(#\Space #\Newline #\Backspace #\Tab
                        #\Linefeed #\Page #\Return #\Rubout))

(defun trim-left (s)
  "Remove whitespaces at the beginning of s. "
  (string-left-trim *whitespaces* s))

(defun trim-right (s)
  "Remove whitespaces at the end of s."
  (string-right-trim *whitespaces* s))


(defun trim (s)
  (string-trim *whitespaces* s))

~~~

### Concat


~~~lisp
(defun concat (&rest strings)
  "Join all the string arguments into one string."
  (apply #'concatenate 'string strings))
~~~

### Join

Snippets on the old cookbook or stackoverflow advised to use a
`format` construct. Which is weird, and causes problems if your
separator contains the `~` symbol.

~~~lisp
(defun join (separator strings)
  (let ((separator (replace-all "~" "~~" separator)))
    (format nil
            (concatenate 'string "~{~a~^" separator "~}")
            strings)))
~~~

Now:

~~~lisp
(is "foo~bar"
    (join "~" '("foo" "bar")))
~~~

### Split

`cl-ppcre` takes a regexp, but we don't need this for the basic cases
of `split`. And disabling this regexp was not straightforward:

~~~lisp
(defun split (separator s &key omit-nulls)
  "Split s into substring by separator (cl-ppcre takes a regex, we do not)."
  ;; cl-ppcre:split doesn't return a null string if the separator appears at the end of s.
  (let* ((val (concat s
                      (string separator)
                      ;; so we need an extra character, but not the user's.
                      (if (string-equal separator #\x) "y" "x")))
         (res (butlast (cl-ppcre:split (cl-ppcre:quote-meta-chars (string separator)) val))))
    (if omit-nulls
        (remove-if (lambda (it) (empty? it)) res)
        res)))
~~~

Now: `(split "." "foo.bar")` just works.


### Repeat

~~~lisp
(defun repeat (count s)
  "Make a string of S repeated COUNT times."
  (let ((result nil))
    (dotimes (i count)
      (setf result (cons s result)))
    (apply #'concat result)))
~~~

### Replace-all

This required to use cl-ppcre and one switch of it to avoid regexps.

~~~lisp
(defun replace-all (old new s)
  "Replace `old` by `new` in `s`. Arguments are not regexs."
  (let* ((cl-ppcre:*allow-quoting* t)
         (old (concatenate 'string  "\\Q" old))) ;; treat metacharacters as normal.
    (cl-ppcre:regex-replace-all old s new)))
~~~


### starts-with? start string

The Lisp way was to check if the beginning of "string" contains
"start", taking its length, dealing with corner cases,…

~~~lisp
(defun starts-with? (start s &key (ignore-case nil))
  "Return t if s starts with the substring 'start', nil otherwise."
  (when (>= (length s) (length start))
    (let ((fn (if ignore-case #'string-equal #'string=)))
      (funcall fn s start :start1 0 :end1 (length start)))))

;; An alias:
;; Serapeum defines a "defalias".
(setf (fdefinition 'starts-with-p) #'starts-with?)

(defun ends-with? (end s &key (ignore-case nil))
  "Return t if s ends with the substring 'end', nil otherwise."
  (when (>= (length s) (length end))
    (let ((fn (if ignore-case #'string-equal #'string=)))
      (funcall fn s end :start1 (- (length s) (length end))))))

(setf (fdefinition 'ends-with-p) #'ends-with?)
~~~

Usage illustrated by the tests:

~~~lisp
(subtest "starts-with?"
  (ok (starts-with? "foo" "foobar") "default case")
  (ok (starts-with? "" "foo") "with blank start")
  (ok (not (starts-with? "rs" "")) "with blank s")
  (ok (not (starts-with? "foobar" "foo")) "with shorter s")
  (ok (starts-with? "" "") "with everything blank")
  (ok (not (starts-with? "FOO" "foobar")) "don't ignore case")
  (ok (starts-with-p "f" "foo") "starts-with-p alias")
  (ok (starts-with? "FOO" "foobar" :ignore-case t) "ignore case"))
~~~

### Predicates: empty? blank?

There was no built-in to make those differences.

~~~lisp
(defun empty? (s)
  "Is s nil or the empty string ?"
  (or (null s) (string-equal "" s)))

(defun emptyp (s)
  "Is s nil or the empty string ?"
  (empty? s))

(defun blank? (s)
  "Is s nil or only contains whitespaces ?"
  (or (null s) (string-equal "" (trim s))))

(defun blankp (s)
  "Is s nil or only contains whitespaces ?"
  (blank? s))
~~~


### words, unwords, lines, unlines

Classic stuff:

~~~lisp
(defun words (s &key (limit 0))
  "Return list of words, which were delimited by white space. If the optional limit is 0 (the default), trailing empty strings are removed from the result list (see cl-ppcre)."
  (if (not s)
      nil
      (cl-ppcre:split "\\s+" (trim-left s) :limit limit)))

(defun unwords (strings)
  "Join the list of strings with a whitespace."
  (join " " strings))

(defun lines (s &key omit-nulls)
  "Split the string by newline characters and return a list of lines."
  (split #\NewLine s :omit-nulls omit-nulls))

(defun unlines (strings)
  "Join the list of strings with a newline character."
  (join (make-string 1 :initial-element #\Newline) strings))
~~~





### Substring

The builtin `subseq` is much poorer compared to what we have in other languages.

Take Python, we can do:

    "foo"[:-1] # negative index and starting from the end
    "foo"[0:100] # end is too large, thus it returns the entire array.

This was not possible with `subseq`, it throws a condition. Nothing
found in Alexandria or other helper libraries.

~~~lisp
(defun substring (start end s)
  "Return the substring of `s' from `start' to `end'.

It uses `subseq' with differences:
- argument order, s at the end
- `start' and `end' can be lower than 0 or bigger than the length of s.

- for convenience `end' can be nil or t to denote the end of the string.
"
  (let* ((s-length (length s))
         (end (cond
                ((null end) s-length)
                ((eq end t) s-length)
                (t end))))
    (setf start (max 0 start))
    (if (> start s-length)
        ""
        (progn
          (setf end (min end s-length))
          (when (< end (- s-length))
            (setf end 0))
          (when (< end 0)
            (setf end (+ s-length end)))
          (if (< end start)
              ""
              (subseq s start end))))))
~~~

Usage:

~~~lisp
(subtest "substring"
  (is "abcd" (substring 0 4 "abcd") "normal case")
  (is "ab" (substring 0 2 "abcd") "normal case substing")
  (is "bc" (substring 1 3 "abcd") "normal case substing middle")
  (is "" (substring 4 4 "abcd") "normal case")
  (is "" (substring 0 0 "abcd") "normal case")
  (is "d" (substring 3 4 "abcd") "normal case")
  (is "abcd" (substring 0 t "abcd") "end is t")
  (is "abcd" (substring 0 nil "abcd") "end is nil")
  (is "abcd" (substring 0 100 "abcd") "end is too large")
  (is "abc" (substring 0 -1 "abcd") "end is negative")
  (is "b" (substring 1 -2 "abcd") "end is negative")
  (is "" (substring 2 1 "abcd") "start is bigger than end")
  (is "" (substring 0 -100 "abcd") "end is too low")
  (is "" (substring 100 1 "abcd") "start is too big")
  (is "abcd" (substring -100 4 "abcd") "start is too low")
  (is "abcd" (substring -100 100 "abcd") "start and end are too low and big")
  (is "" (substring 100 -100 "abcd") "start and end are too big and low")
  )
~~~

### See also

and afterwards I saw
[cl-strings](https://github.com/diogoalexandrefranco/cl-strings) which
does help but can have its shortcomings.

The Cookbook is updated: https://lispcookbook.github.io/cl-cookbook/strings.html
