---
title: "How to Get a File Size (and others Posix Attributes like its mtime) in Common Lisp"
date: 2017-06-16T16:26:00+02:00
draft: false
---

There is nothing built-in since CL predates the posix standard.

After a look at
[Awesome CL](https://github.com/CodyReichert/awesome-cl), the
[Osicat library](https://www.common-lisp.net/project/osicat/manual/osicat.html)
was my go-to package to look for such functionnality. There is its
`osicat-posix` package indeed, even though it is undocumented
([issue](https://github.com/osicat/osicat/issues/20))…

Now a look at the [Cookbook](https://lispcookbook.github.io/cl-cookbook/files.html#getting-file-attributes-size-access-time-with-the-osicat-library) is ok.

## osicat, osicat-posix

`osicat-posix` is included in `osicat`.

    (ql:quickload :osicat)


~~~lisp
(describe (osicat-posix:stat #P"/tmp/file"))

#<OSICAT-POSIX:STAT {1004F20C93}>
  [standard-object]

Slots with :INSTANCE allocation:
  DEV      = 2065
  INO      = 7349974
  MODE     = 33204
  NLINK    = 1
  UID      = 1000
  GID      = 1000
  RDEV     = 0
  SIZE     = 4304
  BLKSIZE  = 4096
  BLOCKS   = 16
  ATIME    = 1497626097
  MTIME    = 1497347216
  CTIME    = 1497347216
; No value
~~~

and so we can access the slots with their related functions:

~~~
osicat-posix:stat-dev
osicat-posix:stat-gid
osicat-posix:stat-ino
osicat-posix:stat-uid
osicat-posix:stat-mode
osicat-posix:stat-rdev
osicat-posix:stat-size
osicat-posix:stat-atime
osicat-posix:stat-ctime
osicat-posix:stat-mtime
osicat-posix:stat-nlink
osicat-posix:stat-blocks
osicat-posix:stat-blksize
~~~

so for example:

~~~lisp
(let ((stat (osicat-posix:stat #P"./files.md")))
  (osicat-posix:stat-size stat))  ;; => 10629
~~~


## Trivial-file-size

Now for the size there's also the lightweight (and portable)
[trivial-file-size](https://github.com/TBRSS/trivial-file-size).

> This library exports a single function, file-size-in-octets. It returns the size of a file in bytes, using system calls when possible.

> The canonical way to determine the size of a file in bytes, using Common Lisp, is to open the file with an element type of (unsigned-byte 8) and then calculate the length of the stream. This is less than ideal. In most cases it would be better to get the size of the file from its metadata, using a system call.

The author new about osicat-posix.
