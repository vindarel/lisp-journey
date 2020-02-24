---
title: "Why do we have to wait one month for Quicklisp updates ?"
date: 2017-08-31T18:14:37+02:00
draft: false
---

If you didn't know that, now you do. Quicklisp releases software
updates once a month (see
[Quicklisp's blog](http://blog.quicklisp.org/)). I didn't know why, it
isn't explained on its website, so I asked
([issue #148](https://github.com/quicklisp/quicklisp-client/issues/148)). I
found the discussion very insightfull, everybody being constructive,
existing solutions being discussed and architectural choices
explained. But it ended up brutally with one more Common Lisp oddity.

My first impression was that this fact is annoying, because it already
prevented me a couple of times to use my own library and its most
recent updates into other projects. They would pull the lib from
Quicklisp but wouldn't benefit from its latest features.

This way of doing was also not the package management model I was most
used to (pip, npm,…), but by wording things differently it makes more
sense to me. As an user says:

> I think it is a very unique to quicklisp, making sure that everything compiles together. I can't think of any other libraries/frameworks system on other languages/platforms that would go as far. Really great work.

So *Quicklisp is more than a package manager*, it is a "dist" builder
ensuring everything works together, closer to apt than to pip.


The situation and shortcomings is well described by axity on
[his blog post](https://www.axity.net/blog/article/8) (now showing a
404):

> Zach Beane has done an excellent job with Quicklisp, and it is far better than what we previously had available, but still has a few problems. Zach puts a lot of effort into curating a list of compatible software together into the form of a “Quicklisp dist” rolled out approximately every month. While this is great and puts near-zero maintenance on developers, it poses a few problems.

>    Firstly, Zach is a single point of failure. Yes, anyone can maintain their very own Quicklisp dist, but it isn't going to see the masses, and the Quicklisp internals are not very well understood by anyone other than Zach.

>   Also the fact that the official dists are rolled out so far apart (a month or longer in the software world is an eternity), means developers cannot push hot-fixes or address user-reported bugs in a timely manner, without pushing maintenance onto the users by having them checkout upstream sources.

>   Modern languages such as Julia and Racket offer central repositories where a developer can register their software projects, and will be automatically indexed periodically, so that users can continue to install and update software without any maintenance, and still receive updates quickly when needed. Additionally, they push managing version dependencies onto the developer, which I do not believe to be a bad thing. In constrast, Common Lisp libraries are rarely versioned, and all of that maintenance is forced upon the Quicklisp dist curator.

I also like /u/ruricolist's explanations
([on reddit](https://www.reddit.com/r/lisp/comments/6snw5d/questions_for_2017_common_lisp_experts/dljcuz9/)):

> Quicklisp provides more assurance than you might expect. The criterion for inclusion, and maintenance, of a project in Quicklisp is that the project successfully load in SBCL alongside all the libraries in that Quicklisp distribution, and load without any errors or warnings. SBCL has extensive type inference and can catch and warn about many potential issues at compile time. And because of the pervasive use of macros in CL, successfully loading a library usually exercises a lot of code paths in its dependencies. To a surprising extent, "if it loads, it runs."

>   Qlot isn't something a library would use. You use it to set up the dependencies for an application. My approach (with TBRSS) is this. Obviously, every time I upgrade to a new Quicklisp dist, I run a test suite to make sure everything is working together. On the rare occasion there's a problem, I either pin the offending library to an older version (with Qlot) or I fork it and fix it, again using Qlot to pull from the fork until the fix makes it into the next Quicklisp dist. And of course I also use Qlot for dependencies that are not, for whatever reason, available in Quicklisp.

---

So Quicklisp's author answers:


> The work relying on me is that I build everything in Quicklisp to make sure they build together before making a release. This covers a useful class of bugs. I'd love to incorporate more tests in the process to catch release problems that don't manifest at build time.


> I hope to make it easier for people to learn how to make their own dists. Then people will have the opportunity to make new software sets following the policies that are most important to them. I think there's also plenty of room for other package managers for Common Lisp - maybe something styled more like clbuild would suit people who want instant access to updates.

Why a month ?

> A month between updates is a compromise between chaos and stability.

> I don't think shorter release cycles are an unqualified good. In my experience, short release cycles can lead to instability and unpredictability, and I chose one month as a balance between getting timely updates and having a reliable, stable base.

> The most labor-intensive part of making releases is monitoring daily failures and reporting bugs to the right people. The daily failure report is automated, but reporting bugs (and following up) can be a slog.

Then the discussion began. When voices support more frequent releases,
Zach advertises again
[quicklisp-controller](https://github.com/quicklisp/quicklisp-controller);
I read one (and only one) small but direct criticism towards the
maintainer:

> @Hexstream I don't think a fork will be required. I think this is less of an issue with @xach not willing to make changes required by the community and more of lack of manpower that's willing and able to take up the burden of building and maintaining an automated Quicklisp dist/repository.

A one month update has its supporters, of course:

> I have benefited multiple times from the 1 month release cycle and the due diligence that Xach and others put in every month. Even if they had a team of 50 I'd still vote to keep it as it is now. Making a separate dist is far more sensible if you need extra control over delivery times and it will still play well with quicklisp and your other projects.

I asked whether it would be possible to specify a git version, and I had an explanation:

> Regarding pulling from git in the client, Quicklisp doesn't work like that. I did not want to rely on external processes in order to be portable to all Common Lisp implementations and platforms. This was a real issue in clbuild and asdf-install.

> If you want to update a bugfix of your library, fix it, and wait a month.

and the reminder to try [Qlot](https://github.com/fukamachi/qlot),
which allows exactly that (and to set dependencies locally).

> Qlot will not help if you are writing a library and want to push fixes to its users quickly. It only helps for end-user application development. It works similar to virtualenv + pip (requirements.txt) from Python's world, for example.

Phoe summed up the situation:

> This thread is turning into a discussion about "why X approach is better than Quicklisp approach" which leads to a fruitful, but dead point.

>If someone is not satisfied with the way the current Quicklisp dist works and would rather have a more automated solution, then they are free to extend the quicklisp-controller to their liking and implement the required functionality for:

>-  creating a centralized service that acts as a QL repository and dist manager,
>-  allowing the authors to upload and/or update their projects on that service,
>-   setting up a CI test loop on that service for verifying that the packages build,
>-   automatically updating the service's dists with new releases,
>-   maintaining all of the above.

>Until such a person or group of people appears, nothing is going to be be achieved and nothing in Quicklisp is going to change.

>Talk is cheap - @xach has at least built something that works and can act as a foundation.

And now, after only 17 messages to the thread, Zach Bean closes the thread:

> I can appreciate there are other approaches that have advantages over how Quicklisp works. I hope this thread has helped shed some light on why it works the way it does, and my hopes for the future. I'm not against requests for changes, but not all of them can or will be accommodated. I'm also fully in favor of people doing their own thing if they have other priorities, experiences, and preferences - I think it would be great if there were even more options for Common Lisp project management.

> Closing this for now - thanks for the discussion.

Hexstream had just the time to disagree

> Generally agree with your last comment, but I just wanted to express my discontent at the premature closing of this thread, it seemed pretty fruitful to me and I don't think it had yet reached a point of serious diminishing returns.

> (Your project, your rules, though.)

and this is it.


---

I think, and I'm not the only one, that the thread was fruitful.

Indeed, we had explanations (that don't appear in the doc), we had
presentation of means to the resolution (in no doc), we had the
presentation of how to fix the mentioned problem for library
developers (Qlot, comprehensibly not referenced in Quicklisp doc, and little talked
about over here), we had questions regarding the lack of documentation
that could be tracked from this thread (Zach too said he wanted to
write more doc). And I think the discussion was professional, with no
animosity. So we could have tracked some progress, maybe we would have
received more tips, but more importantly we'd have been done with this
question.

But the thread is closed O_o Preventing people from communicating
means preventing people from learning from each
other, and thus makes the CL world evolving slower. Or people
quitting, or simply be very surprised and not staying here. Indeed,
it's a negative feeling to see that. Why did Zach close the thread ?
Is he bored of this discussion ? He can ignore it. Bored of being
asked that ? It's a recurrent question in reddit and in blogs. But
this issue is the only mention of the subject on Quicklisp's website and
repository. Closing it makes it much less visible.

Thus it is more likely newcomers will ask again. It's sure newcomers
won't learn why Quicklisp works like it does, which appears in good
light in that issue (and below in reddit). What happened is again a
thing that makes the CL world impenetrable (or with great effort) and
subject to rants. It doesn't need more reasons, seriously.

I wouldn't bother if this subject was documentet, but it is not. The
issue about documenting Quicklisp, which Zach wanted to fix "soon",
stalls since 2014. It's still open, at least.

---

Final links, with a glimpse of light:


* [the reddit thread](https://www.reddit.com/r/Common_Lisp/comments/6x7c85/why_do_we_have_to_wait_one_month_before_quicklisp/)

* [Qi](https://github.com/CodyReichert/qi), a Common Lisp package
  manager in the making - more traditionnal, no surprises - didn't try.
