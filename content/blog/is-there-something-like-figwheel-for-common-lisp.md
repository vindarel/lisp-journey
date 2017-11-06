---
title: "Is There Something Like Clojure's Figwheel for interactive web dev with the browser in Common Lisp ?"
date: 2017-05-04T12:32:01+02:00
draft: false
---

Looks like there is:
[trident-mode](https://github.com/johnmastro/trident-mode.el), an
*"Emacs minor mode for live Parenscript interaction"*, based on
[skewer](https://github.com/skeeto/skewer-mode) but: trident-mode
doesn't seem to be used in the wild (while skewer-mode is) and I don't
know Figwheel so all I can say is that it seems a bit different:
instead of letting us selectively evaluate and send code to the
browser, Figwheels seems to rebuild the entire project and send the
result when we write a file.

I tried trident-mode quickly, it works and the author was
responsive. It offers commands and shortcuts to see the Javascript
code produced by Parenscript forms and (optionally) send them to the
browser.

An example use:

~~~lisp
((@ document write)
  (ps-html ((:a :href "foobar") "blorg")))
~~~

to evaluate with `trident-eval-dwim`, which generates

    document.write("<A HREF=\"foobar\">blorg</A>")

so it uses js to insert html into the DOM.
It doesn't leverage Skewer's capacity to send only html.


I'll update this post if/when I can.

---

[Figwheel](https://github.com/bhauman/lein-figwheel)

>  builds your ClojureScript code and hot loads it into the
>  browser as you are coding!

Skewer

> Provides live interaction with JavaScript, CSS, and HTML in a web browser. Expressions are sent on-the-fly from an editing buffer to be evaluated in the browser, just like Emacs does with an inferior Lisp process in Lisp modes.

and we can also connect to sites on servers we don't control.

They have demo videos.
