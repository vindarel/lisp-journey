---
title: "Lisp Interview: Arnold Noronha of Screenshotbot: from Facebook and Java to Common Lisp."
date: 2021-12-06T19:08:30+01:00
tags: ["companies",]
draft: false
---

I have come to like asking questions to people running companies that
use CL, and I have Arnold in my radar for quite some time.

He contributed a while back to my
[cl-str](https://github.com/vindarel/cl-str/) library, and at that
time, I don't recall how many Lisp projects he had in his Github, but
not as much as today. Since then, he created
[ScreenShotBot](https://screenshotbot.io/) (an open-source screenshot
testing service) and he released a few very useful Lisp (and Elisp)
libraries. Wow! I'll try to investigate what happened :)

![](https://screenshotbot.io/assets/images/integrations/botty.png "Screenshotbot")

## First, ScreenShotBot. It's an open-source project and a company. Its website shows a team of four. Can you tell us what's the state of the project, of the company, what are your goals with it? Who are your clients?

Sure. So the team of four isn't fully active at the moment. We're all
still friends, but Screenshotbot didn't take off the way we wanted it
to, so currently it's just me and Reuben.  I'm the primary developer,
Reuben [^1] is actually my brother, he manages marketing. But he's tech
savvy. I'll talk about how he contributes in code without knowing any
Lisp in a second.

Lili [^2] and Francesco [^3] are awesome designers we worked with in
building our marketing pages.

Reuben and I are also prototyping some other ideas at the moment.

By the way, I think cl-str was probably one of the first CL libraries I
contributed to. It's also easily my most used library :)

[^1]: https://www.linkedin.com/in/reuben-noronha/?miniProfileUrn=urn%3Ali%3Afs_miniProfile%3AACoAAAsEnhABsKiB51XHsvofsDRNmKm8mKEaxOg
[^2]: https://www.linkedin.com/search/results/all/?keywords=lilibeth%20bustos%20linares&origin=RICH_QUERY_SUGGESTION&position=0&searchId=11575c64-025c-46c3-b812-c8f19e298680&sid=ZY7
[^3]: https://www.linkedin.com/in/francescostumpo/


## You pinned the project [facebook/screenshot-tests-for-android](https://github.com/facebook/screenshot-tests-for-android) on your GH profile. Maybe you wrote about it already. What's your relation with this project? (we read you "built the infrastructure for running Screenshot Tests at Facebook, which still runs tests from iOS, Android and React at Scale.")

I was the original author of that project when I worked at
Facebook. It's the de-facto screenshot testing library for
Android. (The other one, called Shot, uses this library under the
hood.) It's maintained by other engineers at Facebook at the moment.

At Facebook, I also ended up building the infrastructure to run these
screenshot tests, and that infrastructure ended up being used for iOS
and React and ElectronJS etc. The infrastructure stored the
screenshots, bisected, sent tasks, notified on diff reviews etc.

With Screenshotbot I wanted to build a similar infrastructure that can
be used outside of Facebook.

## So… do you come from Facebook and from Java ? (ah, you worked at Google too!)

Yes Java! Also did C++ and lots of Python at Facebook.

## What made you start a new project/company, and…

Speaking of Java.. Java is ridiculously slow to work with. Especially
on Android, every single change required you to re-build the entire
app. My initial goal when I started my own company was to solve this
problem. I wanted to build CL style interactive development for Java,
but without forcing developers to switch from Java. (Take a look at an
early demo here: https://www.jipr.io/demo).

I haven't completely abandoned this project, I might get back to this
soon. But this is a good segue to your next question.

## (the famous question) how did you end up with Common Lisp?

So I've been toying with Lisp for a while. I started using StumpWM
around 2010, and have been using it ever since (I think I was looking
for Emacs level customizability for my WM which CL and StumpWM gave
me). My own personal website runs on CL, probably since about
2015. Using CL encouraged me to build small tools for personal use
without much friction. (For instance, I have my own weight tracker
tool, tracking my car mileage, tracking my investment portfolio etc.)
I just had my server running on my desktop, and Emacs was perpetually
connected to the Lisp server, so making changes would take less than a
minute.

Even before I started using CL professionally, I attempted to build a
shitty Lisp interpreter way back in 2012 [^4]. The goal here was mostly to
learn about compilers.

Now, when I started working on Jipr, at some point I realized I needed
to define some kind of "bytecode". But then I thought, why not just
use Lisp as the "bytecode", i.e. the Java gets compiled to Lisp? I
needed a Lisp-like language that works with Android and Java to build
the actual Java interpreter. At that time, I could not find any CL
that provided me the ability, so I boldly took the Lisp that I built
in 2012, and actually built a shit tonne of real code on top of
it. (The demo I linked to earlier used my homegrown Lisp.)

At some point, that stopped scaling. Every small change required lots
of debugging because of the lack of SLIME, and other debugging
tools. I gave up for a while, until one day I realized I could
actually pay LispWorks, and they have already built Android support
for me.

*Paying for Lispworks was the best thing I've done in my startup
journey*. Initially, I just wanted the Android support for building
Jipr, and I was able to remove all references of my homegrown Lisp
with a month of effort. Their support is spectacular. And having
access to Java libraries meant I could move a lot faster on my future
ideas, including Screenshotbot.

[^4]: https://github.com/tdrhq/mini-lisp


## Are you the only (Lisp) developer on Screenshotbot, did you have to make a colleague work on the Lisp codebase and how did that go?

I'm the solo Lisp developer. However: I've worked on multiple websites
collaborating with my wife (who has some experience with Python, but
not technical), and my brother (who is tech savvy, but not a
programmer). Both helped me build websites in a Lisp codebase because
of [Markup](https://github.com/moderninterpreters/markup)! *I helped them set up Atom + SLIMA* (with SBCL, not
Lispworks since I couldn't afford another license), *and then since it
looked like HTML from that point, they would mostly work with a theme
that we would purchase to build complex UIs*.

When they took over the UI, that left me more time to work on building
the core components and interactions for the apps.


## We can find you on reddit. Besides it, is there another Lisp circle where you are active? (a pro mailing list?) Do you have interaction with the "pro" side of the CL community?

Hmm, I'm definitely active on Reddit. I follow the LispWorks HUG, but
I'm not a super active participant. Do you have suggestions of where
else I should be active on?

## If I recall correctly, you use LispWorks, and you appreciate its Java FFI. Can you give details on your stack? When do you turn to the Java side?

I briefly described this above. For Jipr, which is literally a Java
interpreter, Java interop was a necessity (the interpreter which runs
in Lisp, keeps calling back and forth into Java). And it helps that
Lispworks also has Android support (which CCL and ABCL don't have, the
other two implementations that support Java).

For Screenshotbot, the app has multiple integrations with externals
tools such as Slack, Asana, etc. Sometimes, interacting with the
services' APIs aren't that hard, and can be written in plain old
Lisp. But often, I need to prototype a solution quickly, and having
access to existing well-tested Java libraries are a necessity. (For
instance, I had a few sales calls where the client was using a
specific external tool for which I didn't have the integration, and I
would have to work overnight to build the integration).

Over-reliance on Java can eventually cause pain and suffering (I have
to restart the Lisp process if I make a Java change), so I avoid
writing any Java code, and instead talk to the Java libraries
directly, even if it's slower. Usually these Java libraries are not in
the hot-path anyway, so performance isn't critical. Occasionally I
rewrite code that relied on Java to just use plain CL.


## Anymore feedback on LispWorks? It seems you also extensively use Emacs and Slime.

Yeah, I definitely don't use the LispWorks IDE. :) I tried it for a
while, but it wasn't for me. Sly (I prefer Sly over Slime these days),
is definitely a more "complete" experience for me.

I'm the kind of hacker that is always optimizing their workflow. Emacs
just makes it way more easier to do that. [Slite](https://github.com/tdrhq/slite/) is obviously one
of my bigger examples, but there are always smaller things. (My most
recent one was automatically guessing which package to import a symbol
from, so I can press `C-c i` on a symbol, and it'll give me a list of
package candidates using `ido-completing-read`.)

Perhaps there are ways to do this with the LispWorks IDE, but I'm more
experienced with Emacs Lisp, and there's a wealth of documentation and
blogs about how to do different things in Emacs.

## How's your CI and deployment story going, is everything fine, is there anything particular to CL?

I have a mono-repo. Almost all my Lisp code is in one single
repo. Some of my newer open source libraries get copied over from the
mono-repo to GitHub using Copybara.

I run my own Jenkins server, but plan to switch to Buildbot. Jenkins
is annoying beyond a certain point. But it's a great starting point
for anybody who doesn't have CI.

I have two desktops, and use my older desktop exclusively as a Jenkins
worker. But Lispworks adds a complication because of licensing issues,
so my primary desktop runs all the Lispworks jobs.

Deployment is interesting. I heavily use bknr.datastore [^6], which while
awesome, adds a little pain point to deployment. Restarting a service
can take a 10-20s because we have to load all the objects to
memory. So I avoid restarting the service.

Lispworks also adds a quirk. A deployed Lispworks image doesn't have a
compiler, so I can't just `git pull` and `(asdf:load-system ...)`. So
instead, from my desktop I build a bundled fasl file, and have scripts
to upload it to my server and have the server load the fasl file.

Finally, bknr.datastore has some issues which reloading code, which
causes existing indices to become empty. I haven't debugged why this
happens, but I'm close. I have workaround scripts for these that can
correct a bad index, but because of the potential of bringing my
datastore into a bad state, deployment is pretty manual at the moment.

[^6]: an object-persistence library with transactions for CL: https://github.com/hanshuebner/bknr-datastore/ See also this [good tutorial on Medium](https://ashok-khanna.medium.com/persistent-in-memory-data-storage-in-common-lisp-b-k-n-r-37f8ae76042f).


## Some CL libraries you particularly like? Some you wish existed?

Apart from the usual suspects (cl-str, alexandria, cl-json,
hunchentoot etc):

- bknr.datastore: This is a game changer. If you're the type of person
that likes prototyping things quickly, this is for you. If you need to
scale things, you can always do it when you have to. (In my experience,
not all ideas reach the stage where you need to scale over a 100
servers.) It does take a bit of getting used to (particularly dealing
with how to recover old snapshots, or replaying bad transactions). I'm using my own patches for Lispworks that aren't merged upstream yet
(I have a PR for it). I think the index failing issue might be in my
own patches, but I don't know for sure yet.

- [dexador](https://github.com/fukamachi/dexador/) (as opposed to Drakma): For the longest time I avoided Dexador
because I thought Drakma is the more well established library. But
Drakma for all its history, still doesn't handle SSL correctly at
least on LispWorks. And dexador does.

- [Qlot](https://github.com/fukamachi/qlot) also sounds awesome, and I want to start using it.

Some I wish existed:

- A carefully designed algorithms library, with all the common
algorithms, and consistent APIs.

- ...in particular graph algorithms. There are libraries out there, and
I have used them, but they are clunky and not very well documented.

- Better image processing libraries. Opticl is fine, but confusing to
work with. Maybe it's just me.

- A modern and complete Selenium library. I use the Java library with
the Java FFI.

- An extensible test matchers library, similar to Java's
Hamcrest. There's a cl-hamcrest, but it's not very extensible, and you
can't configure the test result output IIRC. I attempted a solution
here (copied over as part of my mono-repo):
https://github.com/screenshotbot/screenshotbot-oss/tree/main/src/fiveam-matchers,
but it's not ready for publishing yet.

I also think there's a verbosity problem to be solved with classes and
methods, that still needs to be solved in the Lispy way. For instance,
in Java or python, the method names don't have to be explicitly
imported, but in CL we have to import each method that we need to use,
which makes it hard to define what the object's "interface" is. I am
not proposing a solution here, I'm just identifying this as a problem
that slows me down.


## Is that a baby alligator you caught yourself on your GH profile picture?

It's one of those pictures they take of you at a tourist trap alligator tour. :)

The alligator's jaw is taped shut.

## Anything more to add?

Nothing I can think of :)

---

Thanks and the best for your projects!

notes:

- Arnold's GitHub: https://github.com/tdrhq/
- ModernInterpreters: https://github.com/moderninterpreters
