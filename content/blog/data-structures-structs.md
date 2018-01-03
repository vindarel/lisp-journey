+++
date = "2018-01-03T07:51:49+01:00"
title = "Structures: lightweight records, a step before classes"
draft = false
+++


Structures offer a way to store data in named slots. They support
single inheritance.

Classes provided by the Common Lisp Object System (CLOS) are more flexible however structures may offer better performance (see for example the SBCL manual).

As usual, this is *[best read in the Common Lisp Cookbook](https://lispcookbook.github.io/cl-cookbook/data-structures.html#structures)*.

## Structures

### Creation

`defstruct`

~~~lisp
(defstruct person
   id name age)
~~~

At creation slots are optional and default to `nil`.

To set a default value:

~~~lisp
(defstruct person
   id
   (name "john doe")
   age)
~~~

Also specify the type after the default value:

~~~lisp
(defstruct person
  id
  (name "john doe" :type string)
  age)
~~~

We create an instance with the generated constructor `make-` +
`<structure-name>`, so `make-person`:

~~~lisp
(defparameter *me* (make-person))
*me*
#S(PERSON :ID NIL :NAME "john doe" :AGE NIL)
~~~

note that printed representations can be read back by the reader.

With a bad name type:

~~~lisp
(defparameter *bad-name* (make-person :name 123))
~~~

```
Invalid initialization argument:
  :NAME
in call for class #<STRUCTURE-CLASS PERSON>.
   [Condition of type SB-PCL::INITARG-ERROR]
```

We can set the structure's constructor so as to create the structure
without using keyword arguments, which can be more convenient
sometimes. We give it a name and the order of the arguments:

~~~lisp
(defstruct (person (:constructor create-person (id name age)))
     id
     name
     age)
~~~

Our new constructor is `create-person`:

~~~lisp
(create-person 1 "me" 7)
#S(PERSON :ID 1 :NAME "me" :AGE 7)
~~~

However, the default `make-person` does *not* work any more:

~~~lisp
(make-person :name "me")
;; debugger:
obsolete structure error for a structure of type PERSON
[Condition of type SB-PCL::OBSOLETE-STRUCTURE]
~~~



### Slot access

We access the slots with accessors created by `<name-of-the-struct>-` + `slot-name`:

~~~lisp
(person-name *me*)
;; "john doe"
~~~

we then also have `person-age` and `person-id`.

### Setting

Slots are `setf`-able:

~~~lisp
(setf (person-name *me*) "Cookbook author")
(person-name *me*)
;; "Cookbook author"
~~~

### Predicate

~~~lisp
(person-p *me*)
T
~~~

### Single inheritance

With the `:include <struct>` argument:

~~~lisp
(defstruct (female (:include person))
     (gender "female" :type string))
(make-female :name "Lilie")
;; #S(FEMALE :ID NIL :NAME "Lilie" :AGE NIL :GENDER "female")
~~~

### Limitations

After a change, instances are not updated.

If we try to add a slot (`email` below), we have the choice to lose
all instances, or to continue using the new definition of
`person`. But the effects of redefining a structure are undefined by
the standard, so it is best to re-compile and re-run the changed
code.

~~~lisp
(defstruct person
       id
       (name "john doe" :type string)
       age
       email)

attempt to redefine the STRUCTURE-OBJECT class PERSON
incompatibly with the current definition
   [Condition of type SIMPLE-ERROR]

Restarts:
 0: [CONTINUE] Use the new definition of PERSON, invalidating already-loaded code and instances.
 1: [RECKLESSLY-CONTINUE] Use the new definition of PERSON as if it were compatible, allowing old accessors to use new instances and allowing new accessors to use old instances.
 2: [CLOBBER-IT] (deprecated synonym for RECKLESSLY-CONTINUE)
 3: [RETRY] Retry SLIME REPL evaluation request.
 4: [*ABORT] Return to SLIME's top level.
 5: [ABORT] abort thread (#<THREAD "repl-thread" RUNNING {1002A0FFA3}>)
~~~

If we choose restart `0`, to use the new definition, we lose access to `*me*`:

~~~lisp
*me*
obsolete structure error for a structure of type PERSON
   [Condition of type SB-PCL::OBSOLETE-STRUCTURE]
~~~

There is also very little introspection.
Portable Common Lisp does not define ways of finding out defined super/sub-structures nor what slots a structure has.

The Common Lisp Object System (which came after into the language)
doesn't have such limitations. See the [CLOS section](https://lispcookbook.github.io/cl-cookbook/clos.html).

* [structures on the hyperspec](http://www.lispworks.com/documentation/HyperSpec/Body/08_.htm)
* David B. Lamkins, ["Successful Lisp, How to Undertsand and Use Common Lisp"](http://www.communitypicks.com/r/lisp/s/17592186045679-successful-lisp-how-to-understand-and-use-common).
