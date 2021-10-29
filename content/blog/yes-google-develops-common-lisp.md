---
title: "Yes Google Uses and Hacks on Common Lisp"
date: 2020-03-02T20:47:32+01:00
tags: ["google special tag", "sbcl", "companies",]
draft: false
---

[ITA Software](https://en.wikipedia.org/wiki/ITA_Software), owned by
Google, the airfare search and pricing system that is still used by
companies such as Kayak.com or Orbitz, is a well-known example of a
successful industrial and large Common Lisp software.

We're legitimate to wonder if they still run it (they do), if Google
develops more CL software (I don't know), or if they put resources to
improve a CL implementation: they do.

According to https://mstmetent.blogspot.com/2020/01/sbcl20-in-vienna-last-month-i-attended.html:

> Doug Katzman talked about his work at Google getting SBCL to work with Unix better. For those of you who don't know, he's done a lot of work on SBCL over the past couple of years, not only adding a lot of new features to the GC and making it play better with applications which have alien parts to them, but also has done a tremendous amount of cleanup on the internals and has helped SBCL become even more Sanely Bootstrappable. That's a topic for another time, and I hope Doug or Christophe will have the time to write up about the recent improvements to the process, since it really is quite interesting.

> Anyway, what Doug talked about was his work on making SBCL more amenable to external debugging tools, such as gdb and external profilers. It seems like they interface with aliens a lot from Lisp at Google, so it's nice to have backtraces from alien tools understand Lisp. It turns out a lot of prerequisite work was needed to make SBCL play nice like this, including implementing a non-moving GC runtime, so that Lisp objects and especially Lisp code (which are normally dynamic space objects and move around just like everything else) can't evade the aliens and will always have known locations.


So now that's in the wild, Common Lisp can go trendy again: Google uses and dedicates resources for Common Lisp!
