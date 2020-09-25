---
title: "Composing queries with Mito, or doing without Django lazy querysets and Q objects"
date: 2020-09-24T18:15:44+02:00
draft: false
---

When I didn't know Lisp at all, I skimmed at CLSQL's and Mito's
documentation and I didn't find a mention of "lazy", "querysets" (a
Django term!) nor a mention of any means to compose queries. I had no
idea how I would replace `querysets`, `F` and `Q` objects and the many
functions for DB queries that were being added into newer Django
versions. I concluded that the Lisp ecosystem was lagging behind.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [The solution](#the-solution)
- [Last words](#last-words)

<!-- markdown-toc end -->


Then I began to understand. And today I got the chance to rewrite a
Django query involving querysets and `Q` objects, using regular
Lisp. All you have to know is back-quote and comma.

We implement a simple search into a DB. The user enters one or more
words and we search against the `title` and the `authors` fields. We
want to match all words, but each can be either in the title, either
in the authors field.

Considering we have two products:

```ascii
1 - The Lisp Condition System - phoe
2 - Implementing a blockchain in Lisp - F. Drummer
```

then searching for "lisp cond" must return one result. DWIM.

In Python, we must use `Q` objects to "OR" the terms with `|` (you can't use `|` without `Q`):

```python
products = firstquery()
for word in words:
    products = products.objects.filter(Q(title__icontains=word) |
                                       Q(authors__name__icontains=word))\
                               .distinct()
```

The promise of `filter` is to be lazy, so when we chain them the ORM
 constructs one single SQL query.

So what does this query yield as SQL? Funnily, I didn't find a
built-in way to get the generated SQL and I had to use a third-party
library. Mmh, I could use special logging. The fact is, we are far
from SQL here (and, with the experience, it is NOT a good thing).

It looks like this (searching "hommes femmes" in our test DB):

```sql
       SELECT DISTINCT ... FROM "product" LEFT OUTER JOIN ...
       WHERE
       (("product"."title" LIKE %hommes% ESCAPE '\'
         OR "author"."name" LIKE %hommes% ESCAPE '\')
        AND
         ("product"."title" LIKE %femmes% ESCAPE '\'
         OR T5."name" LIKE %femmes% ESCAPE '\'))
       ORDER BY "product"."created" DESC
       LIMIT 3
```

Does that look complicated? Does that need alien "Q objects"?! It's just a AND around two OR:

```ascii
   title like keyword 1 OR author like keyword 1
AND
   title like keyword 2 OR author like keyword 2
```

Mito is the high-level library, and we compose queries with SXQL. I
already had a little query that worked with one keyword:

```lisp
(defun find-product (&key query (order :asc))
  (mito:select-dao 'product
    (when query
      (sxql:where (:or (:like :title (str:concat "%" query "%"))
                       (:like :authors (str:concat "%" query "%")))))
    (sxql:order-by `(,order :created-at))))
```

If `:query` is given, we filter the search. If not, the `when` is not executed and we return all products.

So what we need to do is iterate over the keywords, produce as many OR
and wrap them with a AND. We want something like that (we can try in the REPL):

```lisp
(:AND
 (:OR (:LIKE :TITLE "%word1%")
      (:LIKE :AUTHORS "%word1%"))
 (:OR (:LIKE :TITLE "%word2%")
      (:LIKE :AUTHORS "%word2%")))
```

So:

## The solution

```lisp
(sxql:where
 `(:and   ;; <-- backquote
   ,@(loop for word in (str:words query)  ;; <-- comma-splice
        :collect `(:or (:like :title ,(str:concat "%" word "%"))  ;; <-- backquote, comma
                       (:like :authors ,(str:concat "%" word "%"))))))
```

(using `(ql:quickload "str")`)

Pay attention to `,@` (comma-splice). Without it, we get a bad level
of nesting and two parenthesis before the :OR. We would get a list of
clauses, instead of each clause individually. You can try in the REPL.

Note: if you are uneasy with back-quote and comma, see: https://lispcookbook.github.io/cl-cookbook/macros.html

## Last words

Django's `filter` [is similar to using a
When](https://docs.djangoproject.com/en/1.8/ref/models/conditional-expressions/),
which we were already using on the Lisp side without knowing it was
anything special. "Q objects" are easy to replace. So, Python and
Django might be easy to getting started with (or it is your feeling,
because you must learn the special syntax and its limitations, I bet
you had some "WTF?!" moments), but comes a time when your application
grows that you pay the price of being far from SQL (not counting the
maintenance cost).

With Mito and SXQL, it's all regular Lisp, we are closer to the metal,
the only limitation being to know the language, and a bit of SQL.

So we have a great example of why some Common Lisp libraries have a
surprisingly low number of commits. You know, that little voice in
your head that wonders if a library is finished or active. The author
might not need to develop feature X, thanks to Lisp's
expressiveness. Likewise, many questions don't need to be asked or
upvoted on Stack-Overflow. Though I should have asked years ago.

---

- getting started with a DB: https://lispcookbook.github.io/cl-cookbook/databases.html
- more DB choices: https://github.com/CodyReichert/awesome-cl#database

---

<div>
You like what I'm doing?
<br/>

<a href='https://ko-fi.com/K3K828W0V' target='_blank'><img height='36' style='border:0px;height:36px;' src='https://cdn.ko-fi.com/cdn/kofi2.png?v=2' border='0' alt='You can buy Me a Coffee at ko-fi.com' title='My hidden plan is to make Common Lisp popular again. No fixed income ATM, and I want to do more, so it helps. Thanks!'/></a>

</div>
