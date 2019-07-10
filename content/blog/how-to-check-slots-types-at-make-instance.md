---
title: "How to Check Slots Types at make-instance"
date: 2019-07-10T15:28:08+02:00
draft: false
---

In CLOS, a slot can have a `:type` option, but it doesn't inforce type
checking. It is good practice to use it, for documentation and for
compiler optimizations and warnings sometimes (with CCL and SBCL when
safety is high), but one shouldn't rely on it. To comply this need, we
can simply create our own constructor functions.

However, the
[sanity-clause](https://github.com/fisxoj/sanity-clause) library can
do it since a couple of days. The validation error messages are
pretty good.  Demonstration.

> Sanity clause is a data validation/contract library. You might use it for configuration data, validating an api response, or documents from a datastore. In a dynamically typed langauge, it helps you define clearly defined areas of doubt and uncertainty. We should love our users, but we should never blindly trust their inputs.

> To make use of it, you define schemas, which can be property lists with symbols for keys and instances of :class:sanity-clause.field:field

We define a class `person` with slot options from sanity-clause:
`:field-type`, `type` `:members`, `:required`:

~~~lisp
(defclass person ()
  ((favorite-dog :type symbol
                 :field-type :member
                 :members (:wedge :walter)
                 :initarg :favorite-dog
                 :required t)
   (age :type (integer 0)
        :initarg :age
        :required t)
   (potato :type string
           :initarg :potato
           :required t))
  (:metaclass sanity-clause.metaclass:validated-metaclass))
~~~

Now we try to create a `person` with `make-instance`, but we give a bad dog name:

~~~lisp
(make-instance 'person :favorite-dog :nope)
; Evaluation aborted on Error converting value for field #<MEMBER-FIELD {1004BFA973}>:
Value "NOPE" couldn't be found in set (WEDGE WALTER)
~~~

Now with a bad age:

~~~lisp
(make-instance 'person :age -1 :favorite-dog :walter)
; Evaluation aborted on Error validating value -1 in field #<INTEGER-FIELD {1004BFF103}>:
* Value -1 didn't satisfy condition "must be larger than 0"
~~~

When a required field is missing:

~~~lisp
(make-instance 'person :age 7 :favorite-dog :walter)
; Evaluation aborted on A value for field POTATO is required but none was provided..
~~~

And well, it works when all is OK :]

~~~lisp
(make-instance 'person :age 1 :favorite-dog :walter :potato "patate")
#<PERSON {10060371E3}>
~~~

The usual warnings apply: it's a new library, we must try it and use
it with caution. It however opens up more possibilities. It would be
awesome to couple it with an ORM like Mito. This is an [open
issue](https://github.com/fisxoj/sanity-clause/issues/8).

Resources:

- https://stackoverflow.com/questions/51723992/how-to-force-slots-type-to-be-checked-during-make-instance/56920918
- https://lispcookbook.github.io/cl-cookbook/clos.html#slot-type
