---
title: "I Worked Remotely in Common Lisp. Here's My Incredible Story."
date: 2020-05-12T12:52:57+02:00
draft: false
---


Nearly one year ago, I received an email that asked me if I was
available to do remote Lisp work. It was the day before the end of a
contract and I had to tell my team if I wanted to continue or not. I
made a virtual offering to the Lisp god and I started the Lisp job.

---

Disclaimer: this post was written on [Lisp Advocates'
reddit](https://www.reddit.com/r/lispadvocates/comments/fopbgn/i_have_worked_in_common_lisp_remotely_heres_my/). Lisp Advocates is a meme, but it's sort of serious too.

---
At this time I had been in Lisp for around two years, contributing a
couple simple libraries, writing a lot of documentation, blogging,
furnishing the reddits, and being enthusiastic and polite. This is
what actually gave me the job. I had tried to contribute to a busy CL
repository, but the PR was not good enough and that irritated the
maintainer, who answered abruptly. Nothing's more outrageous than
receiving contributions right? But I answered with calm and
professionalism, and that got noticed by a repository watcher, who
decided he could work with me.

That guy already had contacts and a client, and he formed a team
around him. Our work was to build a website that would receive many visitors,
that would have a client registration form and would
have a rather simple admin dashboard, for a team of half a dozen
people. The business already existed in the form of a buggy and slow
Wordpress site, so the expectations were clear. We were three, we
worked together on the same code (with one guy more on the design). I
worked on it in a two-months period, but not full time. I've had a
decent income paid straight and so I paid my rents for a few months
thanks to that experience.

What Lisp was good for

The application had no inherent difficulties. It had forms and an
admin backend. It was a website for a team of commercial people, as it
exists hundreds of thousands. And yeah, Common Lisp was suited for
that task. So we see there's a good margin of progression, business
and remote work wise: those thousands of websites for commercial
people can very well be done in CL.

Libraries, deployment and Lisp curse

We picked the Caveman framework, the Mito ORM and the cl-markup
templating library, with some tests in FiveAM. There was a little bit
of JavaScript, less than a thousand lines. I find Caveman a bit
convoluted but it was clear and easy. I like Mito very much and wrote
material for it. I liked to play with the web server debugging options: usually I
received the stacktraces in the debugger in my editor, but I could
choose to display them on the browser (as I'm used with Django or
Flask). It is this time that I enjoyed so much being able to change
the faulty function, recompile it, choose the "try again" restart and
see the operation succeed. Now when I'm back on Python I feel the Lisp
curse. I'll never be the same. I'll never enjoy Python as much as
before. Sigh. Anyways, we deployed the app on DigitalOcean with Fast
CGI, as documented on Caveman's README.

The bug

Our most difficult bug that made us loose millions was due to
`(string-downcase nil)` to return "NIL", the string, instead of
`nil`. Now I use my [str](https://github.com/vindarel/cl-str/) library
for string manipulation purposes.

All in all, being able to live-debug the software from the earth
proved invaluable.

I got also hit by a config of mine that impacted Mito's results. I had
set `*print-case*` to `:downcase` in my .sbclrc. I was asking Lisp to
DON'T SHOUT AT ME ALL DAY LONG, 'cause I try to listen to music at the
same time. I fixed the Mito bug, but I don't use this setting anymore.

Voilà. This is my response to LispAdvocates' call: https://www.reddit.com/r/lispadvocates/comments/ficdvx/tell_us_you_remote_success_story/.

There are of course lots of situations were CL is ready now to get the (remote) job done. There are people who do web dev for years in CL, but we don't know their story.

Share yours!

ps: stay tuned, 'cause I deployed another website in production.

---

Some comments and answers:

> Apart from the programmer experience, were there any inherent advantages to using Common Lisp? (Speed I guess?)

CL had no particular advantages, but no disadvantages either (and it
is my point!). As I said, it was a site with basic/easy/HTML&JS
requirements, so I believe no language would've had any particular
advantage. Speed was important, it was one of the main
requirements. The website felt responsive, the client was very happy
about it. For us, it was also easy and fast to deploy, which turned
important and impressed the client.

> Do you thinK a more standardized framework (from other languages) could have saved you?

No, not with our requirements. Another framework&language would have
make us loose millions at the very beginning (still figuratively)
