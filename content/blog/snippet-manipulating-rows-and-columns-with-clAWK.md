---
title: "Snippet: Manipulating Rows and Columns With ClAWK"
date: 2019-11-04T18:40:52+01:00
draft: false
tags: ["libraries"]
---

I just discovered [clawk](https://github.com/sharplispers/clawk), that
seems to originate from
[lispbuilder-clawk](https://github.com/lispbuilder/lispbuilder). Its
last commit dates from 2011, typical from Lisp and that's OK,
libraries have the right to be done, it has no useful README nor
documentation, but we can see its use in [the
tests](https://github.com/sharplispers/clawk/blob/master/clawktest.lisp),
and the library is easily discoverable.

This library seems perfect to manipulate data in rows and columns.

Let's have a quick look with this dummy txt file:

```
1 Alice 40 40
2 Bob 39 50
```

I had a conflict when `use`-ing clawk, which I resolved by not
accepting the change in the debugger.

We parse all lines, give a name to the space-delimited fields, and print them back:

```lisp
(for-file-lines ("test.txt")
    (with-fields ((a b c d))
        ($print a b c d)))

1 Alice 40 40
2 Bob 39 50
NIL
```

Let's multiply the two last fields. If we use the regular `*` operator, we get a
type error because fields are extracted as strings by default. We then
use `$*`:


~~~lisp
(for-file-lines ("test.txt")
    (with-fields ((id name payrate hrsworked))
        (declare (ignore id))
        ($print name ($* payrate hrsworked))))
Alice 1600
Bob 1950
NIL
~~~

We can change the field separator with a string or a regexp, with the
no surprising `clawk:*fs*` variable (`FS` in awk):

~~~lisp
(for-file-lines ("test.txt")
    (let ((clawk:*fs* "-"))
        (with-fields ((a b c))
        ($print a))))
~~~


And… that's all folks. Another tool to keep in our toolbelt.

- more about awk: https://www.gnu.org/software/gawk/manual/html_node/
