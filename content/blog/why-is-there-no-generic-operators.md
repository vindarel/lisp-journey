---
title: "Why is there no generic operators ?"
date: 2017-04-14T16:27:44+02:00
tags: ["tip", "libraries",]
draft: false
---

*TLDR;* because the object system came afterwards (and it was not the
 intention to make CL entirely object oriented).

As a CL enthousiast coming from Python, I feel the pain not to have
generic or polymorphic operators but having to learn about many
specialized operators instead. Why is it so and are there solutions ?

---

I [asked](https://stackoverflow.com/questions/43416293/why-is-there-no-generic-operators-for-common-lisp) on SO.


In CL, there are many operators to check for equality that depend on
the data type: `=`, `string-equal`, `char=`, then `equal`, `eql` and
whatnot, so on for other data types, and the same for comparison
operators. There are no generic and extendible operators. For our
own types, we define its own functions.

As a reminder for those equality operators: `equal` does work on
integers, strings and characters and `equalp` also works for lists,
vectors and hash tables an other Common Lisp types but objects. See
[the SO answers](https://stackoverflow.com/questions/43416293/why-is-there-no-generic-operators-for-common-lisp)
for precisions and experienced lispers debatting of the inner
subtilities and traps of those functions.

The language has mechanisms to create generics though, see
[generics (defgeneric, defmethod)](http://www.gigamonkeys.com/book/object-reorientation-generic-functions.html)
as described in Practical Common Lisp.

There have been work in that direction (https://common-lisp.net/project/cdr/document/8/cleqcmp.html) but no library as of today.

It's a recurrent concern, also
[this blog post](https://www.reddit.com/r/programming/comments/65ct5j/a_pythonist_finds_a_new_home_at_clojure_land/)
("Not a monad tutorial", great serie) points to this. The guy moved
to Clojure, for other reasons too of course, where there are only one (or
two?) equality operators.

So it seems that the reason is mostly historical, the object system
(CLOS) appearing afterwards. Of course the generics would be
slower. But how much slower ? I really don't care, as a beginner and
for web stuff.

> Generic CLOS functions were added several years after CL was originally designed (82-84). The variant with CLOS was widely published with CLtL2 (1990) and then ANSI CL. The language was only slightly updated. It was not the intention to make Common Lisp fully object oriented. Also performance of CLOS for relatively low-level functions is kind of problematic. Dylan, which is something like Scheme + CLOS - s-expression syntax, did this: it defines more of the language in terms of generic functions. [Rainer Joswig on SO]


### The CL21 way

Fortunately [CL21](http://cl21.org/) introduces (more) generic
operators, particularly for sequences it defines `length`, `append`,
`setf`, `getf`, `first`, `rest`, `subseq`, `replace`, `take`, `drop`, `fill`,
`take-while`, `drop-while`, `last`, `butlast`, `find-if`, `search`,
`remove-if`, `delete-if`, `reverse`, `reduce`, `sort`, `split`,
`join`, `remove-duplicates`, `every`, `some`, `map`, `sum` (and some
more). Those should work at least for **strings**, **lists**,
**vectors** and extend the new `abstract-sequence` type.

Now CL21 is something worth presenting and debatting in another post.

More:

- https://github.com/cl21/cl21/wiki
- https://lispcookbook.github.io/cl-cookbook/cl21.html
