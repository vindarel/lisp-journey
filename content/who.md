+++
date = "2022-04-05T07:51:49+01:00"
title = "Who's using Common Lisp ?"
draft = false
+++


> Everyone says “Nobody uses Lisp” and Lispers say “Yes they do, there’s ITA, and, um, Autocad, and, uh, oh yeah, Paul Graham wrote Viaweb in Lisp!” Not very helpful for either side.

We now have a list: [**awesome-lisp-companies**](https://github.com/azzamsa/awesome-lisp-companies/). It isn't official nor exhaustive, but it's way better than the past situaton.

Of course, see also:

* [lisp-lang.org's success stories](http://lisp-lang.org/success/) for
  a showcase of projects and companies in aerospace, AI & Machine
  Learning, Science, Graphics etc.
* [Franz.com success stories](https://franz.com/success/)
* [LispWorks' success stories](http://www.lispworks.com/success-stories/index.html)

In a nutshell, what's Common Lisp good for? Let's quote Kent Pitman's famous answer:

> But please don't assume this is an exhaustive list, and please don't assume Lisp is only useful for Animation and Graphics, AI, Bioinformatics, B2B and Ecommerce, Data Mining, EDA/Semiconductor applications, Expert Systems, Finance, Intelligent Agents, Knowledge Management, Mechanical CAD, Modeling and Simulation, Natural Language, Optimization, Research, Risk Analysis, Scheduling, Telecom, and Web Authoring just because these are the only things they happened to list. Common Lisp really is a general language capable of a lot more than these few incidental application areas, even if this web page doesn't totally bring that out.

(and this list doesn't mention that it was used for auto-piloting the
Deep Space 1 spaceship by the NASA for several days).

---

Here's a shorter list of companies using CL in production©.

Quantum computing companies use CL, especially SBCL:

 [**Rigetti**](http://rigetti.com/about)

They already sponsored a Quicklisp development. They chose Common Lisp (SBCL). [Video](https://www.reddit.com/r/Common_Lisp/comments/7ifq92/lisp_at_the_frontier_of_computation_by_robert/). *Their* Lisp even runs 40% faster than *their* C code.

<img src="/images/rigetti.png" alt="rigetti" width="750" style="z-index: 100"/>

[**D-wave systems**](http://www.dwavesys.com), "quantum processor development". "The software is implemented in Common Lisp (SBCL) and is an integral part of the quantum computing system." [lispjobs announce](https://lispjobs.wordpress.com/2015/03/16/software-developer-for-quantum-processor-development-group-d-wave-systems-vancouver-british-columbia/).

<img src="/images/dwave.png" alt="dwave" width="750" style="z-index: 100"/>

And there's at least also [HRL Laboratories](https://www.hrl.com/) in the Quantum space (I am not counting defunkt Quantum companies). They are "one of the world's premier physical science and engineering research laboratories. [They] engage in the development of a full quantum device and computation stack, including an optimizing compiler for a quantum programming language". Uses SBCL.


[**Grammarly**](https://tech.grammarly.com/blog/posts/Running-Lisp-in-Production.html) is a famous English language writing-enhancement platform.

<img src="/images/grammarly.png" alt="grammarly" width="750" style="z-index: 100"/>

[**Ravenpack**](https://www.ravenpack.com/) is "the leading big data analytics provider for financial services". [reddit announce](https://www.reddit.com/r/Common_Lisp/comments/7ldiyg/suggestion_for_common_lisp_internship/).

<img src="/images/ravenpack.png" alt="ravenpack" width="750" style="z-index: 100"/>

[**ITA Software**](https://www.itasoftware.com/) is still Google's
leading airfaire search and scheduling platform. They use SBCL, and
contribute to its development.

<img src="/images/ita.png" alt="ita" width="750" style="z-index: 100"/>

[**Kina knowledge**](https://www.kinaknowledge.com/) is a small company that "automates the processing of documents": "We use Common Lisp extensively in our document processing software core for classification, extraction and other aspects of our service delivery and technology stack". See their [Lisp Interview: questions to Alex Nygren](https://lisp-journey.gitlab.io/blog/lisp-interview-kina/)

<img src="https://user-images.githubusercontent.com/3721004/138441191-375aa5dc-5a82-44e0-a175-61e97536cee6.png" style="max-width: 750px"/>


**Doremir Music Research AB** develops [ScoreCloud](https://scorecloud.com/), a music notation software (a LispWorks product). It lets you play an instrument, sing or whistle into the app, and it writes the music score. Futuristic indeed.

<img src="/images/scorecloud.png" style="max-width: 750px"/>


I'll stop here and I'll let you discover the [**awesome-lisp-companies**](https://github.com/azzamsa/awesome-lisp-companies/) list!
