---
title: "How to access url query parameters in Clack, Lucerne or Caveman"
date: 2017-05-04T12:59:30+02:00
draft: false
---

If you're using Lucerne don't search more like I did, its
`with-params` macro works with url query parameters (as well as POST
parameters).

If you're accessing the url `hello?search=kw`, this works:

~~~lisp
@route app "/hello"
(defview index (name)
  (with-params (search)
    (render-template (+index+)
                     :search search)))
~~~

An illustration with a POST parameter from the "utweet" example:

~~~lisp
@route app (:post "/tweet")
(defview tweet ()
  (if (lucerne-auth:logged-in-p)
      (let ((user (current-user)))
        (with-params (tweet)
          (utweet.models:tweet user tweet))
        (redirect "/"))
      (render-template (+index+)
                       :error "You are not logged in.")))
~~~

The macro is implemented like this:

~~~lisp
;; https://github.com/eudoxia0/lucerne/blob/master/src/http.lisp
(defmacro with-params (params &body body)
  "Extract the parameters in @cl:param(param) from the @c(*request*), and bind
them for use in @cl:param(body)."
  `(let ,(loop for param in params collecting
               `(,param (let ((str (parameter *request*
                                              ,(intern (string-downcase
                                                        (symbol-name param))
                                                       :keyword))))
                          (if (equal str "")
                              nil
                              str))))
     ,@body))
~~~

For Caveman it is [possible](https://github.com/fukamachi/caveman#structured-querypost-parameters) but a bit awkward and [inconsistent](https://github.com/fukamachi/caveman/issues/22).

There's an example for Ningle
[on the related StackOverflow question](https://stackoverflow.com/questions/43778570/how-to-get-url-query-parameters-in-clack-lucerne-or-caveman).

### And in Clack generally ?

It is only scarcely documented on Clack's
[api documentation](http://quickdocs.org/clack/api#package-CLACK.REQUEST).

We can access the parameters with `(clack.request:query-parameter
lucerne:*request*)`. So to get the value of a given param:

    (assoc "a-param" (clack.request:query-parameter lucerne:*request*) :test 'string=)

and this returns the key and the value, so we need another `cdr` to get the value…

~~~lisp
(defun query-param (param)
  (cdr (assoc param (clack.request:query-parameter lucerne:*request*) :test #'string=)))
~~~

See also:

- https://jasom.github.io/clack-tutorial/pages/getting-started-with-clack/
