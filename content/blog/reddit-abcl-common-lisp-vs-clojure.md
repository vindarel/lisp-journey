---
title: "Reddit: ABCL Common Lisp vs Clojure"
date: 2019-09-23T01:47:10+02:00
tags: ["clojure", "abcl"]
draft: false
---

Not that I'm interested in using the Java platform :D but relevant
comparisons between ABCL (Common Lisp on Java) and Clojure are
rare. We just had a nice feedback on reddit, so here it is. The
question was:

> After looking at the quite old benchmarks, ABCL seems to perform alright. Can anyone share their experience with ABCL in terms of performance, stability and memory usage?

----

I wish I could give you more concrete numbers with an application you could test and see for yourself. Since I can't do that, I will tell you about my recent work with ABCL. I ported a small Clojure server-side utility to ABCL and can qualitatively tell you that the performance was close to Clojure. After profiling the ABCL version, I believe I can attribute the differences to ABCL's use of Java reflection for its Java FFI.

I've already been successfully deploying Clojure-based applications professionally, and as I've gotten more into Common Lisp, I'd like to start deploying Common Lisp based applications as well. I recently posted a patch to the ABCL mailing list and got a very quick response from the maintainer. I really like the quality of the ABCL code base. The compiler itself was very approachable and easy to understand.

I think ABCL really is a worthwhile target in the Common Lisp world because:

-    Painless Java FFI. You avoid all the instability and signaling issues that crop up when using the JVM combined with SBCL or CCL. If you make a lot of calls, native Java is always going to be faster anyhow than calls over JNI (which is more comparable to reflection).
-    Use large heaps without worry. Part of the benefit of the JVM is its proven ability to have huge heaps (I've been part of projects that had 64GB+ heaps (though honestly I'd rather stay small)).
-    JVM platform is well supported and tested on a number of OS and hardware platforms

SBCL uses conservative garbage collection and I'm curious how well it would handle really large heaps. CCL uses precise garbage collection but again, I'd like to know how it handles really large heaps. In general, I want all my applications to run with heaps that are naturally in CCL's or SBCL's sweet spot, but I'd love to know I could use ABCL if I really ever needed huge heaps. I'm really getting into Common Lisp because I really like the implementation choices. Having a solid Java FFI unfortunately is usually a requirement in my workplace.

To me, ABCL will be /better/ than using Clojure if ABCL's Java FFI moves away from reflection (when possible). This will close any performance gap with Clojure for most applications. I think this can be done relatively easily in the current ABCL implementation, and I have an idea of how to do it but unfortunately have had no time lately to devote to it. The reason I say "better than Clojure" is that I can write applications that target both ABCL and SBCL/CCL -- I can abstract away my Java APIs if I really have to have them (or use LispWorks with a solid Java FFI if I don't need a ton of Java interoperability). Then when I need fast startup time or low memory footprint, I can use these other CL implementations which are much better suited to it.

The main benefit where I still see Clojure having an edge is if you need a heavy JS-based web interface. I'm not a JS developer, but I was able to successfully use Clojurescript and make a nice looking web application that had pretty seamless interoperability with my Clojure-based server.

Anyhow, I hope this helps you. ABCL is great, I have been very impressed with it and I encourage you to try it out.

---

by the user `somewhat-functional` [on reddit](https://www.reddit.com/r/lisp/comments/d48gcr/how_well_does_abcl_perform/), september 2019
