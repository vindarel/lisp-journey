+++
date = "2018-02-12T07:51:49+01:00"
title = "Fixing a CL21 error message in an unrelated library after a quicklisp update (it's about cache)"
draft = false
+++

I just updated my Quicklisp dist and suddenly couldn't load some
libraries any more. I got an error related to cl21 looking like the one below
(I didn't note the exact message sorry), even though the library was
unrelate to cl21 (it was about osicat and cffi.grovel):

> couldn't find adjustable-vectors from CL21.core.arrays

If you skip through the restarts you'll see mentions of a cache in
`~/.cache/common-lisp/sbclxx-xx/quicklisp/…`. It contains the compiled `.fasl` files.

I deleted this cache and I'm good to go.

---

related: https://github.com/cl21/cl21/issues/99
