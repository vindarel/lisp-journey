---
title: "A free software for bookshops to show their catalogue online"
date: 2020-05-30T13:15:47+02:00
draft: false
---

I wrote a free software for bookshops to publish their catalogue
online. Clients can now browse the available books and order them. It
is enough generic so we can show other products too.

* https://abstock.gitlab.io/#/en/
* sources and bug tracker: https://gitlab.com/vindarel/abstock
* Github mirror: https://github.com/vindarel/ABStock
* [the demo](https://frama.link/syVBKWPw)

Here's how a search result looks like:

<img src="https://abstock.gitlab.io/search.png" style="max-width: 1200px"/>

# Features

The website is made generic enough for different clients, and is made
totally hackable with pre- and post- configuration files that load
your Lisp logic.

By default we get the following pages:

* the welcome screen, with:
  * the bookshop's information,
  * the search form. We can search by title, authors, publisher, shelf and ISBN(s).
  * a random pre-selection of the books to showcase, if enabled.
* an optional special page to showcase a selection of books or other products.
* in the search results page, visitors can add a book to their shopping basket.
* and in the basket page, they find a confirmation form, which sends
  the command by email to the shop owner.

There are obvious TODOs, that could be shocking by their absence, but
that I actually don't need yet, so they'll come right in time :)

* online payment
* admin page
  * simple stats (they are brought in with the email provider, and with Matomo statistics)
* i18n, remove a still few hardcoded words

## Data

ABStock connects by default to the [Abelujo](http://abelujo.cc/en/)
database. Abelujo is a free software for bookshops that I also
develop. Booksellers use it for their daily work, registering and
selling books.

But we can define our own products. The current possibility is
to use a `cards.txt` file. Each block expects a `title`, and that's
the only mandatory field. Other recognized fields are:

```
((:|id| integer)
 :|title|
 :|cover|
 :|isbn|
 :|price|
 :|author|
 :|publisher|
 :|date_publication|
 :|date-publication|
 :|shelf|
 :|shelf_id|
 :|details-url|
 :|summary|
 :|quantity|
 :|repr|
 :|repr2|)
```

We can define other fields, like `likes` below, which is
actually unused in the application.

```
[…]

title: Programming Algorithms
author: Vsevolod Domkin
cover: https://d2sofvawe08yqg.cloudfront.net/progalgs/hero?1586867024
details-url: https://leanpub.com/progalgs
shelf: programming
shelf_id: 99
publisher: Leanpub
price: 15
likes: 5

title: Cats
author: mother cat
cover: https://gitlab.com/abstock/abstock.gitlab.io/-/raw/master/logo.png
details-url: https://gitlab.com/abstock/abstock.gitlab.io/-/blob/master/logo.png
shelf: nature
shelf_id: 98
publisher: nature
price: 5
likes: 100

title: Screwdriver
author:
cover: https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Ftse1.mm.bing.net%2Fth%3Fid%3DOIP.jGOb7dVL1oA9VDzNVPDPpwAAAA%26pid%3DApi&f=1
details-url:
shelf: craftmanship
shelf_id: 97
publisher:
price: 10
likes: 1
```

(yes there's a little redundancy with shelf and shelf_id to fix)

That gives:

<img src="https://gitlab.com/vindarel/abstock/-/raw/master/other-data.png" style="max-width: 1200px"/>

We could very well load a JSON, a CSV or another database when the need arises.

For now, we think the text loader is enough for you to define your
products and try the application.

It is also very easy to host, and in doing so [one can realize that to live-reload his Lisp web app is straighforward and very convenient](https://lisp-journey.gitlab.io/blog/i-realized-that-to-live-reload-my-web-app-is-easy-and-convenient/).

# Context

I shipped the app the second month of our lockdown period for the
client I was working for at that moment, and no need to say it turned
100% helpful. Amazon had hell of an activity, and french alternatives
for booksellers (such as lalibrairie.com or placedeslibraires.fr)
either had stopped, either couldn't accept new registrations. So we
were left alone, and we did that. His clients were happy, they started
passing orders, he organized collection schedules. This happened in a
small rural village, where the inhabitants are happy to have, at
least, a (nice) bookshop in their village.

# Paying one's rent with Lisp

So yes, I paid my rent with Common Lisp again \o/ And you see, the
software is a classical web app. I could have made it with Python or
another language that has a web server and a templating library. As I
defended before, the app doesn't exist thanks to CL's super powers. CL
had no particular advantages for this kind of web app, but no
disavantages either: it has a good web framework, a good templating
library that I liked a lot (Djula: defining custom filters was a
breeze), a good SQL wrapper, and that's all I asked. *I* use CL's
super powers during development and deployment. Clients wouldn't see
the difference… or would they?

Actually, CL has advantages *overall*: development speed, ease of
deployment, ease of hot-reload, ease to use the language features to
bypass a library's limitation (easy-routes had no built-in to
translate a route URL, but it turned possible with a reader macro)…
not mentioning ease of maintenance in time, speed, etc.

One production bug I had was due to (me apart for not testing enough)
`(= 3 nil)` throwing an error, so you must had prior checks (or, I
just realize, use `equalp`?). My Sentry dashboard is empty anyways.


# Final words (with bonus)

The Big Plan is to Rewrite It (the other software) In Lisp, and the
project just moved from R&D to POW… stay tuned, particularly if you
don't know what to do in june and july, I might have a small budget
for a helping hand.

---

<table>
    <tr>
      <td>
<a href="https://www.patreon.com/bePatron?u=35783903" data-patreon-widget-type="become-patron-button">Become a Patron!</a><script async src="https://c6.patreon.com/becomePatronButton.bundle.js"></script>
      </td>

      <td style="padding-left: 1em">
<script src="https://liberapay.com/vindarel/widgets/button.js"></script>
<noscript><a href="https://liberapay.com/vindarel/donate"><img alt="Donate using Liberapay" src="https://liberapay.com/assets/widgets/donate.svg"></a></noscript>
      </td>
   </tr>
</table>
