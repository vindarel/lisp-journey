---
title: "How to do Functional Programming in Common Lisp ?"
date: 2017-07-28T17:38:24+02:00
draft: false
---

or "Common Lisp is not very functional-programming oriented". What are the options ?

I mean, `map` is uncommon and there is no short words like `take` etc
for functional composition. Right ?


**edit** see those [snippets](/blog/snippets-functional-style-more/).

**edit january, 2019**: see this [SO answer](https://stackoverflow.com/questions/54375478/can-lisp-be-easily-used-in-an-immutable-functional-manner/54378903#54378903) and the [modf](https://github.com/smithzvk/modf) library.

### Map and filter

Indeed, there are 8 or so `map` functions. The one we're used to is
`mapcar`. The simple `map` needs a second argument to specify its return
type: `(map 'list (lambda…`. "filter" is named `remove-if-not`.

`mapcan` can also be useful. It concatenates the items into one
list. So with `mapcar` we get a list of lists with our items, with
`mapcan` simply a list of our items.

=> improved in [cl21](http://cl21.org/). It defines the usual `map`
and `keep-if` and more functional verbs: `take`, `drop`, `take-while`,
`drop-while`, `butlast`, `sum`,… in addition to `fill`, `last`,
`find-if[-not]`, `remove-if[-not]`, `delete[-if[-not]]`, `reverse`,
`reduce`, `sort`, `remove-duplicates`, `every`, `some`,…

_Note_: remember that we still have access to `cl` symbols in CL21.


### Functional composition

Lack of functional composition ? There is the
[Series](https://github.com/tokenrove/series/wiki) library since 1989
that seems great, it lets us *"write our program in a functional style
without any runtime penalty at all !"*
[[malisper](http://malisper.me/2016/04/13/loops-in-lisp-part-4-series/)
on his blog post]

But yes again, it has not the modern vocabulary we expect
and it seems abandonware (and its documentation is an old pdf paper
but now hopefully this wiki is better).

CL21 has an operator to compose functions: https://github.com/cl21/cl21/wiki/Language-Difference-between-CL21-and-Common-Lisp#function

### Threading macros (pipes)

We have two packages in Quicklisp:

* [cl-arrows](https://github.com/nightfly19/cl-arrows) defines `->`,
  `->>` and the generalized ones `-<>` and `-<>>`. At the time of
  writing it has two unanswered PRs to add more, like the
  "when-guarded"-"nil shortcuting diamond wand" `some->`.
* [arrow-macros](https://github.com/hipeta/arrow-macros) is a bit more
  complete but has more dependencies (it needs a code walker) which
  are not portable (failed on ECL, Allegro, ABCL, Clisp). It has the
  `some->`, `cond->` and `as->` ones.


### Data structures

We have the nice [FSet](https://github.com/slburson/fset), the functional collection for Common Lisp.

### Anaphoric macros (for shorter lambdas)

I like anaphoric macros but I didn't find one ready to use in a library to write shorter lambdas. The first macro I wrote was directly to mimick elisp's `dash.el` excellent library:

```common-lisp
(defmacro --map (form list)
  `(mapcar (lambda (it) ,form) ,list))
```
so than we can write
```common-lisp
(mapcar (* it 2) '(2 3))
```

instead of `(mapcar (lambda (it) (* it 2) '(2 3)))`.
This macro is very simple. It should be in a library, I don't want to copy-paste it in every project of mine. => in CL21 ? In Anaphora ?

=> Personnally I'm very happy with
[cl21's short lambdas](https://lispcookbook.github.io/cl-cookbook/cl21.html#shorter-lambda)
(that were not documented…):

```
(map ^(foo-bar %) items)
```
or with `(lm (x)…`. Unused arguments will be ignored automatically.

I also quite like the Arc way of doing, that I found in the
[Clamp](https://github.com/malisper/Clamp) project. Arc is another
language and Clamp is more of a POC I guess.

For shorter lambdas we also have
[f-underscore](http://quickdocs.org/f-underscore/api), that I see used
in the wild. It defines some macros to write shorter lambdas:

- `f` is a synonym for lambda
- `f0` is a lambda that takes 0 arguments
- `f_` takes one, accessible in `_` (underscore)
- `f_n` takes one `&rest` argument
- `f_%` ignores its rest argument
- `m` is a lambda "that has a macro-lambda-list instead of an ordinary lambda-list".

You might like [Trivia](https://github.com/guicho271828/trivia) for **pattern matching** too.

Hope this helps !
