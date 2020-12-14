---
title: "Pro mailing list: on Common Lisp and parallel GC"
date: 2020-12-14T22:41:32+01:00
draft: false
---

I recently enjoyed this [discussion on the pro mailing list](https://mailman.common-lisp.net/pipermail/pro/2020-December/thread.html#1837). It started with a call of recommendations on music software, and the discussion evolved in discussing parallel garbage collection. By the way, can you site an implementation that has parallel GC?

---

Pascal Costanza:

«When moving our elPrep software away from Common Lisp, we evaluated C++, Go and Java as potential candidates, and Go turned out to provide the best balance between performance and memory use. We are still using Common Lisp for prototyping, and then translate to Go. These two languages are actually much more similar than it appears at first. […]»

«This was primarily for the lack of good parallel, concurrent garbage collectors in Common Lisp implementations. The CL version of elPrep was actually still a tad faster than any of the C++, Go, or Java versions, but we had to work hard to avoid long GC pauses. elPrep allocates a lot of memory, and the pause time hurts a lot. We solved this by, basically, disabling the garbage collector, and reusing memory manually as much as possible, which turned the program into almost a manually memory-managed affair.»

«Manual memory management became a huge burden because we wanted to add more and more components to the software, and then it becomes almost impossible to predict object lifetimes.»

«We evaluated Go and Java for their concurrent, parallel GCs, and C++ for its reference counting. Interestingly, reference counting is often described as more efficient than GC, but in our case that’s not true: Because there is a huge object graph at some stage that needs to be deallocated, reference counting incurs more or less the same pause that a non-concurrent GC does. That’s why we don’t expect Rust to fare better here either.»

«Again, we’re still prototyping in Common Lisp, which is a huge win, because this makes us much more productive.»

«In my opinion, prototyping in Common Lisp, and then translating to a
different programming language for creating the final product, is a
perfectly valid professional use of Common Lisp. It’s useful to know
which programming languages may be good targets for such an approach. This is, of course, not ideal, because this can easily be
misunderstood as a statement that Common Lisp is not fit for
purpose. However, I don’t see it that way, and you cannot control
people’s perceptions. In our particular case, our manager is on board with this approach, and this allows us to pay for regular licenses for LispWorks. The
approach works really well for us.»

Didier Verna:

«I'd be curious to know if there are particularities in CL itself that make this difficult, or if it's simply because there's no manpower to improve the GCs we have currently.»

Stelian Ionescu:

«It's strictly a lack of manpower. Most CL implementations have GCs that were state-of-the-art 25 years ago: they're either mark-and-sweep or copying & generational, and have to perform all collection while application threads are paused (i.e. stop-the-world), hence the collection pauses that are proportional to the heap size.»

«The newer GCs of Go and the JVM (ZGC and Shenandoah) are not generational and employ techniques such as pointer coloring and store/load barriers by instrumenting all object read/write operations instead of using virtual memory protection (which tends to have a non-indifferent performance penalty), and because they rely heavily on atomic operations to maintain heap consistency the stop-the-world phase is much shorter and only required to update the internal GC metadata.
The result is that instead of 99th percentile pauses of 10+ seconds that we see with QPX or other allocation-heavy applications, these newer GCs show 99th percentile pauses of < 10ms, and perhaps medians going from ~500ms to 2ms (YMMV).»

«Here's a pretty good description of the difference between the two new JVM collectors and how they compare to the older ones: https://www.youtube.com/watch?v=WU_mqNBEacw.»

Martin Cracauer:

«No, it's as possible as in other languages. Some people don't want to pay the overall performance penalty for concurrent GC (as in total CPU time/energy spent for any given piece of work).»

«This particularly applies to applications that are query-based, and hence want to be as fast as possible in the non-GC part, and can GC between queries.  ITA's QPX is an example (although they do desire concurrent GC for better monitoring in the production environment).»

«Parallel GC is no problem and implemented.»

Pascal Costanza:

«Which CL implementations have a parallel GC?»

Jeff Caldwell:

«From Franz's doc on Allegro CL: https://franz.com/support/documentation/10.0/doc/gc.htm#multi-threading-2»

Martin Cracauer:

«Clasp (via Boehm GC and MPS).»

«I thought SBCL was there, but I just checked, not yet.  I think Google
is pushing for a parallel GC instead, because of response times to
their production monitoring.»

«Another untapped source of performance is userfaultfd(2) in the Linux kernel.  It allows those GCs that implement a write barrier using page protections SIGSEGV to use the faster userfaultfd interface instead (as opposed to those using a bitmap).  This won't help concurrent GC, but parallel GC would benefit even more than single-thread GC because it uses faster system calls. Proof of concept is here: https://www.cons.org/cracauer/cracauer-userfaultfd.html»

and:

> Don't the latest incarnations of ECL use the Bohem GC?

Daniel Kochmański:

«They do, we plan to resurrect the homegrown gc as an alternative though.»
