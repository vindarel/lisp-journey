---
title: "Another Common Lisp app in production"
date: 2020-12-14T22:54:28+01:00
draft: false
---

A quick post to celebrate the birth of another Common Lisp application
running in production©. This time, it is not open source, but I can describe it.

It is used by bookshops in France and Belgium to upload there catalogue to online
platforms. And no, they don't know, and don't need to know, the
language it is implemented in!

It is a simple application that reads data from an existing DB, builds
a text file with special rules, sends the file to an FTP server, and
does it every day. I used cl-dbi with raw SQL queries,
[cl-ftp](https://github.com/pinterface/cl-ftp) (does its job perfectly), and a CRON job. I
built a binary that I sent to my server. It is a stand-alone
application that reads a DB that is created by a bigger Python/Django
web app (that I also develop). I didn't want to make this one more
bloated, so given the goals are complementary but orthogonal, I went
with a stand-alone tool.

That's it. One more!

Another tool I am running connects to a SOAP service, shows data on a website (with Sentry configured in production), sells products with Stripe and sends emails with Sendgrid. And I (generally) update it while it runs by connecting to the Lisp REPL. Just throwing out buzzwords to you.

While I'm at it, let me stress one point, to answer in advance a kind
of feedback I already had: no, the resulting application doesn't use
any Lisp superpower and yes, I could have written it in Python. It
turns out Lisp is as suited as Python for this task (or then it is
more suited, since it is faster), the point is *I* benefited from Lisp's
superpowers during development (by using the superior REPL, being able
to build a binary and all that). In conclusion: there are **tons** of
places where Lisp can be used for professional needs out there.

Oh. In doing it, I built those two utilities:

- [progressons](https://github.com/vindarel/progressons), a progress bar that holds on one line and works on the terminal as well as on Slime. It works for me©. My next goal is to make it output a prettier bar with unicode bars.
- [termp](https://github.com/vindarel/termp), a trivial utility that checks if we are on a real or on a dumb terminal (by checking the `TERM` environment variable). So you can `quit` or `error` out.

Two more Lisp repositories on Github !
