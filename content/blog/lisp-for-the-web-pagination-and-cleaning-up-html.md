---
title: "Lisp for the web: pagination and cleaning up HTML with LQuery"
date: 2021-11-25T11:39:18+01:00
draft: false
tags: [web]
---

I maintain a web application written in Common Lisp, used by real
world© clients© (incredible I know), and I finally got to finish two
little additions:

- add **pagination** to the list of products
- **cleanup the HTML** I get from webscraping (so we finally fetch a book
  summary, how cool) (for those who pay for it, we can also use a
  third-party book database).

The HTML cleanup part is about how to use [LQuery](https://github.com/Shinmera/lquery/) for
the task. Its doc shows the `remove` function from the beginning, but I have had difficulty to find how to use it. Here's how. (see [issue #11](https://github.com/Shinmera/lquery/issues/11))

## Cleanup HTML with lquery

https://shinmera.github.io/lquery/

LQuery has `remove`, `remove-attr`, `remove-class`, `remove-data`. It seems pretty capable.

Let's say I got some HTML and I parsed it with LQuery. There are two buttons I would like to remove (you know, the "read more" and "close" buttons that are inside the book summary):

```lisp
(lquery:$ *node* ".description" (serialize))
   ;; HTML content…
        <button type=\"button\" class=\"description-btn js-descriptionOpen\"><span class=\"mr-005\">Lire la suite</span><i class=\"far fa-chevron-down\" aria-hidden=\"true\"></i></button>
        <button type=\"button\" class=\"description-btn js-descriptionClose\"><span class=\"mr-005\">Fermer</span><i class=\"far fa-chevron-up\" aria-hidden=\"true\"></i></button></p>")
```

On GitHub, @shinmera tells us we can simply do:

```lisp
($ *node* ".description" (remove "button") (serialize))
```

Unfortunately, I try and I still see the two buttons in the node or in the output. What worked for me is the following:

- first I check that I can access these HTML nodes with a CSS selector:

```lisp
(lquery:$ *NODE* ".description button" (serialize))
;; => output
```

- now I use `remove`. This returns the removed elements on the REPL, but they are corrcetly removed from the node (a global var passed as parameter):

```lisp
(lquery:$ *NODE* ".description button" (remove) (serialize))
;; #("<button type=\"button\" class=\"description-btn js-descriptionOpen\"><span class=\"mr-005\">Lire la suite</span><i class=\"far fa-chevron-down\" aria-hidden=\"true\"></i></button>"
```

Now if I check the description field:

```lisp
(lquery:$ *NODE* ".description" (serialize))
;; …
;; </p>")
```

I have no more buttons \o/

Now to pagination.

## Pagination

This is my 2c, hopefully this will help someone do the same thing quicker, and hopefully we'll abstract this in a library…

On my web app I display a list of products (books). We have a search
box with a select input in order to filter by shelf (category). If no
shelf was chosen, we displayed only the last 200 most recent books. No
need of pagination, yet… There were only a few thousand books in
total, so we could show a shelf entirely, it was a few hundred books
by shelf maximum. But the bookshops grow and my app crashed once
(thanks, Sentry and cl-sentry). Here's how I added pagination. You can
find the code
[here](https://gitlab.com/vindarel/abstock/-/blob/master/src/pagination.lisp)
and the [Djula](https://mmontone.github.io/djula/) template
[there](https://gitlab.com/vindarel/abstock/-/blob/master/src/templates/includes/pagination.html).

The goal is to get this and if possible, in a re-usable way:

![](/pagination.png)

I simply create a dict object with required data:

- the current page number
- the page size
- the total number of elements
- the max number of buttons we want to display
- etc


~~~lisp
(defun make-pagination (&key (page 1) (nb-elements 0) (page-size 200)
                         (max-nb-buttons 5))
  "From a current page number, a total number of elements, a page size,
  return a dict with all of that, and the total number of pages.

  Example:

(get-pagination :nb-elements 1001)
;; =>
 (dict
  :PAGE 1
  :NB-ELEMENTS 1001
  :PAGE-SIZE 200
  :NB-PAGES 6
  :TEXT-LABEL \"Page 1 / 6\"
 )
"
  (let* ((nb-pages (get-nb-pages nb-elements page-size))
         (max-nb-buttons (min nb-pages max-nb-buttons)))
    (serapeum:dict :page page
                   :nb-elements nb-elements
                   :page-size page-size
                   :nb-pages nb-pages
                   :max-nb-buttons max-nb-buttons
                   :text-label (format nil "Page ~a / ~a" page nb-pages))))

(defun get-nb-pages (length page-size)
  "Given a total number of elements and a page size, compute how many pages fit in there.
  (if there's a remainder, add 1 page)"
  (multiple-value-bind (nb-pages remainder)
      (floor length page-size)
    (if (plusp remainder)
        (1+ nb-pages)
        nb-pages)))
#+(or)
(assert (and (= 30 (get-nb-pages 6000 200))
             (= 31 (get-nb-pages 6003 200))
             (= 1 (get-nb-pages 1 200))))
~~~

You call it:

~~~lisp
(make-pagination :page page
    :page-size *page-length*
    :nb-elements (length results))
~~~

then pass it to your template, which can `{% include %}` the template
given above, which will create the buttons (we use Bulma CSS there).

When you click a button, the new page number is given as a GET parameter. You must catch it in your route definition, for example:

~~~lisp
(easy-routes:defroute search-route ("/search" :method :get) (q shelf page)
   …)
~~~


Finally, I updated my web app (while it runs, it's more fun and why shut it down? It's been 2 years I do this and so far all goes well (I try to not upgrade the Quicklisp dist though, it went badly once, because of external, system-wide dependencies)) (see this [demo-web-live-reload](https://github.com/vindarel/demo-web-live-reload)).

---

That's exactly the sort of things that should be extracted in a
library, so we can focus on our application, not on trivial things. I
started that work, but I'll spend more time next time I need it…  call it
"needs driven development".

Happy lisping.
