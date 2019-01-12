---
title: "Why Deftask Chose Common Lisp"
date: 2019-01-11T12:21:30+01:00
draft: false
---

We heard about [Deftask](https://github.com/deftask/), a task
management app for teams, a few days ago, in an article about the
[internals of the Chronicity library](https://lisper.in/nlp-date-parser). Deftask
uses Common Lisp for its backend and its command-line app. This
reasonates with the fact that [Turtl doesn't use CL anymore](/blog/why-turtl-switched-from-lisp-to-js). So I
asked Deftask's author: why did you go with CL ?

---


More than anything else, I think its down to fun and productivity. I
feel that I am at my most productive when I writing CL in Emacs+SLIME.
It is probably down to the edit-compile-debug cycle which is much
shorter in CL compared to other languages. I originally worked on CL
way back in 2006-08 when I was with Cleartrip. Since then, I have
worked on a number of platforms (frontend js, node, iOS, C, Java,
etc.) but always wanted to go back to writing CL full time.

So when I left my last job a little over a year back, I had already
made up my mind that the next thing I build would be in CL.

The lack of libraries (or rather, well supported libraries) is a
problem, but honestly after over eight years of not working on Lisp,
it doesn't bother me much.

> Did you already build software/services in CL, apart
> the libraries we can see on your github profile ?

As I mentioned I worked at Cleartrip for a little over two years. I
was part of the team that managed the flight search engine,
Unfortunately most of what we did there is gone forever. A small
sliver of our work there resulted in [https://lisper.in/restarts](https://lisper.in/restarts)
(backstory: https://www.reddit.com/r/lisp/comments/7k85sf/a_tutorial_on_conditions_and_restarts/).

> Did you have enough libraries to build your service ? How's deployment and
> maintenance going ?

Well you could say I had enough libraries. Although I still ended up
writing a mini web framework on top of Hunchentoot. Another thing I
wrote is my own Lisp-to-SQL converter. Hope to open source both of
these one day. Apart from that I use drakma, postmodern, djula, plump,
lparallel, cl-json to name a few (alongwith the usual suspects like
alexandria and local-time).

Deployment and maintenance is extremely simple - I just update the
relevant git branch and restart the service. At some point when the
restarts become costly I might add the ability to reload changed code
in the running service.

---


Thanks to him !



Below the backstory from reddit:

---
Hey that was written by me! (Aside: I redirected the page to point to the latest version of this post on my new blog)

Fun fact: I wrote this post way back in 2008 while working for an online travel portal. This was based on some actual work we'd done there. At that time, flight travel in India had started to boom. This I think went hand in hand with a bunch of online travel companies (including ours) gaining a lot of momentum.

To be more competitive, a couple of airlines decided that they wanted to introduce new discounted fares much more frequently than they were doing earlier. The only problem was that they were unable to upload their wonky fare rules in the GDS properly, so they started distributing excel sheets to travel agents with manual instructions on how to apply them.

So our business team starts sending these sheets over to us, and initially, the frequency was low so we just manually hard coded these rules. However then they started sending these sheets every week or so which made our life hell. So we asked asked our business team to "translate" the airline's excel sheets and instructions into a csv, which was subsequently interpreted by a simple rules engine that we wrote.

The only problem? Well, as anyone who's dealt with manually created CSVs will tell you, there were a lot of errors. This didn't really help matters much. We then added a couple of restarts to our CSV parser which allowed us to correct these issues interactively. This made life much better for us -- it was a lot easier than, say, getting a list of errors in a terminal and switching back and forth between the terminal window and the editor to correct them.

Later on we plonked the CSV parser behind hunchentoot and asked our bizdev guy to upload the file there. A handle-bind around the parser collected all the errors in one go and showed them in a nicely formatted way in the browser (see the last section of the post). And so it was no longer our problem :-)

Eventually these airlines decided they wanted to update fare rules daily. Thankfully our "business rules engine" was upto the task. Due to automatic feedback, our friend in bizdev became an expert at uploading the fare rules as soon as they came in. And for quite some time, we were the only ones who could show these cheap fares within minutes of them coming in (if I remember correctly, other portals would take hours to upload the same rules).

Ours was a small team, and we had to manage this in addition to a lot of other things. If it weren't for CL's condition system, I doubt we could have solved this as smoothly as we did. In particular, interactive restarts allowed us (devs) to correct CSV errors without wasting a lot of our own time, and without needing to build a full-fledged UI for a non-dev. And when the time did come for a UI, it was dead easy to write a web fontend on top of it.


> Q: Did you ever work or need an optimal solution to the Tavelling Salesman Problem?

Nope. Domestic flight travel in India is simple... point to point or max over two legs. For international flights we used QPX.

(QPX was ITA software's flight search engine. Probably among the biggest software systems written in Lisp. It now powers Google Flights (I think).)

---
