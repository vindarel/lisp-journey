---
title: "Lisp Interview: more questions to CLPM author. Common Lisp at university for temporal reasoning and risk-bounded planning"
date: 2020-11-15T08:22:59+01:00
draft: true
---

Some days ago [on reddit/r/lisp](https://www.reddit.com/r/lisp/comments/jtla4m/common_lisp_package_manager_a_package_manager_for/),
we got to (re)discover CLPM, the Common Lisp Package Manager.

Its author, Eric Timmons aka [daewok](https://github.com/daewok/), was kind enough to give more context, and to answer some more questions of mine, about his use of Common Lisp in his university group.

Below I'll give an overview of CLPM, stress on how it differs from
Quicklisp, and then paste the interview.

/Note/: it's the same content as on reddit, but saved from oblivion!

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [CLPM](#clpm)
    - [Context](#context)
    - [Feature set and comparison to Quicklisp](#feature-set-and-comparison-to-quicklisp)
    - [More questions to Eric Timmons - using Lisp at university for temporal reasoning and risk-bounded planning.](#more-questions-to-eric-timmons---using-lisp-at-university-for-temporal-reasoning-and-risk-bounded-planning)

<!-- markdown-toc end -->


## CLPM

### Context

> CLPM author here. I'm so happy (and shocked!) that people have found CLPM, especially given how little advertising I've done for it! I'm a PhD student in a group that does a large amount of our coding in Common Lisp. A big part of why I wrote CLPM, for better or worse, is that my group has not done a great job at versioning or maintaining backward compatibility of our various libraries throughout the years. I'm a very applications focused person and it was incredibly frustrating when I needed to deploy some of our code that had worked in the past but found that it bit rotted. And then when I would eventually get everything rolled back to the correct point in time I had no way to release a fix that was no longer applicable on the master branch (normally because the relevant API no longer existed or had been modified in a non-backward compatible way). So I hoped that encouraging/requiring actual version numbers would help us better communicate and reason about our changes over time (and be able to release minor changes to older versions of the code) and the locking capabilities would help save us in situations like giving a demo on short notice.

> I use CLPM as my daily driver as do a couple of the other Lisp-heavy students. It's been going well so far, but I was planning to convert and get feedback from a few more before attempting to spread CLPM beyond my group. That's unfortunately taken a lot longer than I wanted due to COVID, my personal life taking more time than normal (no worries, it's for good reasons!), and just general research related tasks continuing to pop up.

> Now that the cat's out of the bag though, I'd be happy to hear any feedback on it! I'm especially interested in the perspectives of people outside my group since I've been holding their hand in getting it set up and explaining my reasoning/goals to them almost every step of the way.


### Feature set and comparison to Quicklisp

CLPM's [project page](https://gitlab.common-lisp.net/clpm/clpm) (here's a non-official [GitHub mirror](https://github.com/lisp-mirror/clpm), if that helps you browse the repository) lists the project goals. Here's my comment and comparison to Quicklisp.

#### *Support and encourage explicitly versioned systems*

> When a package upgrade introduces regressions, we should be able to use an older version.

But, that is currently not possible with the Quicklisp client, we must refer to other tools (Qlot) or manual workarounds.

CLPM allows to use monthly-based releases, just as Quicklisp and from Quicklisp, but it also started *a new source registry* for Common Lisp libraries, which would:

- allow to *precisely pin dependencies*. It is possible to do so in ASDF, but this propriety is not used in Quicklisp (or barely, or not by most of the library authors, because Quicklisp comes as monthly distributions anyways).
- allow to get the library's home URL, which surprisingly isn't in Quicklisp's metadata (last time I checked, I might be wrong). We have to look at the quicklisp-projects repository.
- it would enforce the libraries to be on version control. Currently Quicklisp also accepts source files (as archives).


#### *Support installing multiple package versions, globally and locally*

CLPM allows to *manage dependencies per project* (per directory), and globally. With Quicklisp, it's only globally. Otherwise we must rely to Qlot, or load projects more manually.

While I personally find the Quicklisp approach great, simple to use,
sufficient in most cases and a better default than always pinning
dependencies manually, comes a point in a software life when we need
project-local dependencies.

#### *Support CI/CD workflows - ship pre-built binaries*

> *CLPM is distributed in both binary and source form*. Source for hackers or people who want to use a different feature set and binary for quick and easy installation in other cases.

That's simpler to install, to use on CI systems, or to make you software's users install the dependencies.

Currently we can use Roswell to install Quicklisp libraries (and software) from the command line, but its installation isn't crystal straightforward or super fast either.

#### *Minimize footprint in development images and deployments*

When you use CLPM and you build a binary of your program, the binary won't contain CLPM (or only if you choose to). When we use Quicklisp, the built image contains Quicklisp (which can be very useful, I use it to live-reload running web apps).

#### *Support HTTPS*

Quicklisp currently doesn't download packages through HTTPS.

Cryptographic signature verification is coming.


## More questions to Eric Timmons - using Lisp at university for temporal reasoning and risk-bounded planning.

### What are you using CL for at your university?

My group's bread and butter is in temporal reasoning and risk-bounded planning and execution (including both activity and path planning). I'm personally working on a high-level language for specifying robotic information gathering missions and an executive to plan and dispatch the missions. Language wise I think you could say it's a stripped down Screamer when it comes to non-deterministic programming and constraints, coupled with parallel and sequential operators and the ability to temporally constrain the relative execution time of different events in the program. There's a few more things, such as support for expressing PDDL-like operators, but that's the 10,000 foot view.

I mentioned I'm applications focused and a lot of that focus of late has been on mission planning for autonomous underwater vehicles. Unfortunately, most of our code is running off the vehicles, but we're slowing moving more reasoning onboard.

### Do you know other teams in your university that are using CL? (or, a lisp-like language?)

I know other groups do but I'm not sure of the details, unfortunately.

### So, why CL? Does it have a killer feature that make your group use it? (it doesn't have to have one though!)

Ha, we started using CL long before I joined the group. From what I hear, it was originally mostly for practical reasons: it's the language my group's PI [Principal Investigator] knows the best and he needed to be able to hop onto any given project as students cycled on and off. But with respect to my personal research, I think CL is the best language for it. You can't beat its macros for defining DSLs and I have a lot of DSLs both in my high level language (along with some MOP code!) and the planner. Something my advisor said to me about CL that really stuck with me is that it is a fantastic language to let you write a new language for the problem you want to solve, and specifying the problem is more than half the battle in solving it.

### Are there downsides, do you have difficulties in certain areas? (formation?)

The biggest downside for us is that students rarely come into the group with CL experience and in rare cases some students refuse to really dive into our Lisp code and stick with something they're more familiar with (such as Python) and end up reinventing poor facsimiles of things that exist.

Ignoring that particular issue, using CL does add a non-trivial amount of time to on-boarding new students. Then beyond that, we had the aforementioned issues with not versioning correctly and not maintaining backward compatibility. While that's really, at it's core, a social issue that would exist regardless of language (and is hard to avoid given the natural turn-over rate of students), the lack of a package manager with features similar to those provided in the languages students come in knowing these days certainly didn't help.

### How did you personally start with CL?

I started with CL when I joined this group. I was given a URL to Practical Common Lisp and off I went. I kind of fell down the rabbit hole at some point and spent more time learning about the language than doing research (oops), but I think that's paid off by this point as I can make CL do nearly anything I can think of. The first draft (or several...) of my code may not be pretty, but they'll work, get the job done, and I can continue working on abstractions and mini-DSLs to my heart's content whenever I need to make things more clear or performant.

### Can you tell us more about your software stack? (implementations, most loved libraries, deployment story (docker?), interface with other tools (monitoring?)…)

We largely use SBCL these days. I routinely try to test on the other big Free implementations as well (ABCL, CCL, ECL) both out of a desire to be portable for portability sake and to make the code more widely useful if we ever get around to sharing it beyond our collaborators more (which I am cautiously hopeful will happen). I particularly love Hunchentoot, Drakma, basically anything from Edi Weitz, log4cl, closer-mop, and, of course, CFFI.

We do a lot of our deployment with Docker (which is why I'm currently maintaining a number of CL related Docker images). I occasionally deploy things using Kubernetes (e.g., when we want to deploy our planners as a service for the students in my advisor's classes to use). I personally love Kubernetes, but I've found that it's difficult to get other students up to speed on it (let alone use it!) because it's just one more set of things for them to learn when their focus is on graduating.

We're also working on getting more of our code running on ARM64 processors, since that's largely what we have available to us for low-power robots. That's proving to be a bit of a challenge, unfortunately because SBCL is fairly memory hungry and our algorithms are also inherently memory hungry. But in the end I think it'll be fine as it's a driving force to get us to do only the necessary reasoning onboard.

We don't have any great stories with regard to interfaces with other tools, but I have been meaning to pick up prometheus.cl and give it a try.

### Anything more to add?

CL is awesome! Nothing else comes to mind right now.

---

Thanks again Eric.

---

<div style='margin-top:80px'>
<a href='https://ko-fi.com/K3K828W0V' target='_blank'><img height='36' style='border:0px;height:36px;' src='https://cdn.ko-fi.com/cdn/kofi2.png?v=2' border='0' alt='ko-fi button' title='Yes, it helps (no fixed nor big income today) and money goes to Lisp development, for me or contributors. Thanks!'/></a>
</div>
