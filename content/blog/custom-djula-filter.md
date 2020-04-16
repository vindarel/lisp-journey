---
title: "Custom Djula filters"
date: 2020-04-08T11:38:17+02:00
draft: false
---

[Djula](https://github.com/mmontone/djula/) is a Common Lisp port of
the Django templating language. It's good, it's proven (it's one of
the most downloaded Quicklisp packages), it is easy to use and it has [good
documentation](http://mmontone.github.io/djula/doc/build/html/index.html).

It basically looks like this:

```html
    {% extends "base.html" %}
    {% block title %}Memberlist{% endblock %}
    {% block content %}
      <ul>
      {% for user in users %}
        <li><a href="{{ user.url }}">{{ user.username }}</a></li>
      {% endfor %}
      </ul>
    {% endblock %}
```

What was missing in the documentation was how to create custom
filters. Here's how, and it's very simple.

## def-filter

Use the `def-filter` macro. Its general form is:

```lisp
(def-filter :myfilter-name (value arg)
  (body))
```

It always takes the variable's value as argument, and it can have one
required or optional argument. For example, this is how those
built-in filters are defined:

```lisp
(def-filter :capfirst (val)
  (string-capitalize (princ-to-string val)))
```

This is all there is to it. Once written, you can use it in your
templates. You can define a filter wherever you want and there is no
need to register it or to import it in your templates.

Here's a filter with a required argument:

```lisp
(def-filter :add (it n)
  (+ it (parse-integer n)))
```

and with an optional one:

```lisp
(def-filter :datetime (it &optional format)
  (let ((timestamp …))))
```

When you need to pass a second argument, make your filter return a
lambda function and chain it with the `with` filter:

```lisp
    (def-filter :replace (it regex)
       (lambda (replace)
         (ppcre:regex-replace-all regex it replace)))

    (def-filter :with (it replace)
       (funcall it replace))
```

Now we can write::

    {{ value | replace:foo | with:bar }}

Note: we should most probably be able to define filters with two
arguments. There's an open issue about that.


## Error handling

Errors are handled by the macro, but you can handle them and return a
`template-error` condition:

```lisp
(def-filter :handle-error-filter (it)
   (handler-case
         (do-something)
     (condition (e)
       (template-error "There was an error executing this filter: ~A" e))))
```

It will be rendered on the browser with a nice stacktrace.


## Final words

If you don't know what template engine to use for your web project,
start with it. My only criticism is that accessing variables is not
totally flexible. The `{{ obj.val }}` syntax already works to access
objects' slots, alists, plists, hash-tables and whatnot (it uses the
excellent
[Access](https://lisp-journey.gitlab.io/blog/generice-consistent-access-of-data-structures-dotted-path/)
library), but it won't work for some data (like structures), forcing
you to a bit of pre-processing before rendering the template. And you
can't use much logic with template tags. However, this is by
design. Djula is a port of the Django templating engine after all.

For more flexible templates and still write html (because, you know, we can copy-paste examples easily!), see [Eco](https://github.com/eudoxia0/eco). See more templates engines in the [Awesome-cl list](https://github.com/CodyReichert/awesome-cl#html-generators-and-templates).

**Last-minute addition**: while I was writing this, Djula's author released [TEN, another templating engine](https://github.com/mmontone/ten), combining the best of Djula and Eco.


- https://mmontone.github.io/djula/doc/build/html/filters.html#custom-filters
