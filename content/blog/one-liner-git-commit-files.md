---
title: "One liner, Git from Lisp: commit every file of this repository"
date: 2018-12-04T21:49:06+01:00
draft: false
---

Hey, pardon this very short post, it's just for the pleasure of
blogging, and to balance the usual lengthy ones.

I wanted to commit, one by one, every file of the current directory
(it's useless, don't ask).

I use [legit](https://shinmera.github.io/legit/) as the interface to
Git, and this one-liner:


~~~lisp
(dolist (file (uiop:directory-files "./"))
   (legit:git-add :paths (pathname file))
   (legit:git-commit :files (pathname file) :message (format nil "add ~a" (file-namestring file))))
~~~

I guessed the `:paths` and `:files` arguments with Slime's command
argument list which appears in the modline, I wanted a function to
convert a `/full/path/file.cl` to a name `file.cl` and tried the
completion for `file-…` and found the right thing without effort. I
saw on the complete documentation that `legit:commit` wanted a
repository object as first argument, which makes sense, but
`legit:git-commit` doesn't and I just iterate on the current working
directory (btw change it in Slime with the `,cd` command) so it was
shorter for me.

Just a one liner.

> Oh my god, I didn't know we can do this in Lisp !

Of course we can :p
