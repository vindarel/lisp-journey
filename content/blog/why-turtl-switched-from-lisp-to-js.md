---
title: "Why Turtl Switched From CL to Js"
date: 2019-01-11T10:21:18+01:00
draft: false
---

Turtl is a very well done, secure collaborative notebook web app.

- https://turtlapp

Its api backend is built in Common Lisp:

- https://github.com/turtl/api/

It is based on many async libraries the developer wrote for Turtl,
like the [Wookie](https://github.com/orthecreedence/wookie) async HTTP
server.

"is" ? No, was :/ Even though this repository is still maintained
(latest commit: 2nd of december 2018), it is deprecated and the
[new server](https://github.com/turtl/server) is written in NodeJS. I
asked Andrew for the reasons behind this, here's his answer.

(in a hurry to spread FUD ? Don't miss this posts's twin,
[Why Deftask chose Common Lisp](/blog/why-deftask-chose-common-lisp)
;) See also some
[companies using Common Lisp](https://github.com/azzamsa/awesome-lisp-companies))

---

It was not an easy decision to replace CL with javascript. I find CL
to be elegant, fast, stable, and above all else, easy to debug (having
the ability to rewrite the program as it's running is insanely
useful). In fact, I still miss lisp and ask myself all the time if I
made the right choice.

I think I did, though.

The path I went with CL was a hard one. I wanted to be able to use
asynchronous programming because for the type of work I do (a lot of
making APIs that talk to other services with very little CPU work)
it's hard to get much more performant. So I embarked on creating
cl-async/cl-libuv (originally cl-libevent) and wookie, along with a
few other drivers. Everything I built worked great (and still works
great, as far as I can tell) however when things did go wrong, there
was nobody to run ideas by and nobody to really help me...I had built
all these things myself, and I also had to be responsible for fixing
them when they broke. On top of having to maintain everything (and it
did break from time to time) there is not much in the way of packages
to help me out. For instance, there's a package to upload files to S3,
but it's not async at all...I had to build this from scratch. There
are more cases of this as well.

With CL, it felt like I was constantly fighting the tide. I was
constantly battling just to get basic things working that are already
solved problems in other languages (Node).

There was help and support from the community along the way, but I was
mostly fighting it alone. I think the straw that broke the camel's
back was when a few people started making copycat projects that added
no real value (other than benchmarking fast) but stole mindshare from
all the work I had put in. It was the "well, that project is not
exactly what I want so I'll make my own from scratch" mindset that
everyone always warned about when I was starting with CL (but I
ignored). I had really hoped the community would have helped propel
the async ecosystem I was building forward, but I just don't think
there's enough people using CL for that to happen.

So between having to maintain everything myself and people putting out
worthless copycat projects that ended up going nowhere, I didn't have
the energy anymore.

Honestly, it took me about a week of work, just nights and weekends,
to reprogram the server in javascript. Granted, most of the "how
should this work?" architecture stuff was already done so it was more
of a rewrite than a build-from-scratch situation, but Node is fast to
build APIs in. I'm decently fluent in javascript and the amount
packages available is so immense that it just made sense.

On top of being fast to build in, it's a well-traveled road. I don't
have people emailing me six times a day asking how to install the
server like I did with CL. I don't have to make weird custom loaders
to run the app on any hosting providers...everyone supports Node. I
don't have to deal with weird FFI errors or libuv nuances. I don't
have to deal with quicklisp's "all or nothing" packaging that doesn't
support version pinning. I don't have to restart the server every 20
days because of some memory leak I have yet to track down somewhere
between cl-libuv, cl-async, wookie, and turtl. There's a whole set of
bullshit I just don't have to deal with anymore.

So I do miss lisp. I'd eventually like to build more things in it
(like games). But I don't think I'll ever touch web stuff in CL again,
and the whole journey left a bitter taste in my mouth. Sure I could
have dropped the async thing and just done a threaded server in
hunchentoot and cl-postgres. But once I decided I was going to
reprogram everything anyway, it just made sense to go with Node.

I took on more work than I could realistically manage, and hoped that
the community would help...but the CL community is small enough that
it was a losing bet and I got burned out.

Hopefully none of this discourages you. CL is a great language. The
community is a mix though. Some of the people in the community are
smart and dedicated, and work on cool projects at a pace they can
maintain. You won't see articles about these projects, and many will
only have a handful of stars on Github (don't measure CL projects by
stars). Seek these projects and these people out, and build things
with them. There is a quiet corner of the internet, with a handful of
people building amazing things in lisp.

---

Before commenting on this, I think we must realize what he achieved,
and that he went the hard way.

Now don't miss [Why Deftask chose Common Lisp](/blog/why-deftask-chose-common-lisp) !
