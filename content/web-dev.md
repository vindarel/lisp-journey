+++
date = "2017-02-05T07:51:49+01:00"
title = "State of Common Lisp Web Development - an overview"
draft = false
+++

¡ THIS IS A DRAFT !

**update july, 5th 2019**: I put this content into the Cookbook: https://lispcookbook.github.io/cl-cookbook/web.html, fixing a long-standing request.

**new post: why and how to live-reload one's running web application**: https://lisp-journey.gitlab.io/blog/i-realized-that-to-live-reload-my-web-app-is-easy-and-convenient/

**new project skeleton**: [lisp-web-template-productlist](https://github.com/vindarel/lisp-web-template-productlist): Hunchentoot + easy-routes + Djula templates + Bulma CSS + a Makefile to build the project


See also the
[Awesome CL list](https://github.com/CodyReichert/awesome-cl#network-and-internet).


Information is at the moment scarce and spread appart, Lisp web
frameworks and libraries evolve and take different approaches.

I'd like to know what's possible, what's lacking, see how to
quickstart everything, see code snippets and, most of all, see how to
do things that I couldn't do before such as hot reloading, building
self-contained executables, shipping a multiplatform web app.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [Web application environments](#web-application-environments)
    - [Clack, Lack](#clack-lack)
- [Web frameworks](#web-frameworks)
    - [Hunchentoot](#hunchentoot)
    - [Caveman](#caveman)
    - [Lucerne](#lucerne)
    - [Snooze](#snooze)
    - [Radiance](#radiance)
    - [cl-rest-server](#cl-rest-server)
    - [Wookie](#wookie)
    - [Weblocks (solving the Javascript problem)](#weblocks-solving-the-javascript-problem)
- [Tasks](#tasks)
    - [Accessing url parameters](#accessing-url-parameters)
    - [Session an cookies](#session-an-cookies)
    - [Data storage](#data-storage)
    - [SQL](#sql)
        - [Persistent datastores](#persistent-datastores)
        - [Migrations](#migrations)
    - [Forms](#forms)
        - [Form validation](#form-validation)
    - [Debugging](#debugging)
    - [Testing](#testing)
    - [Misc](#misc)
        - [Oauth, Job queues, etc](#oauth-job-queues-etc)
- [Templating engines](#templating-engines)
    - [HTML-based](#html-based)
    - [Lisp-based](#lisp-based)
- [Javascript](#javascript)
    - [Parenscript](#parenscript)
    - [JSCL](#jscl)
    - [Ajax](#ajax)
    - [The case Webblocks - Reblocks, 2017](#the-case-webblocks---reblocks-2017)
- [Shipping](#shipping)
    - [Building](#building)
    - [Multiplatform delivery with Electron (Ceramic)](#multiplatform-delivery-with-electron-ceramic)
- [Deployment](#deployment)
    - [Running the app on a web server](#running-the-app-on-a-web-server)
        - [Manually](#manually)
        - [with [Clack](http://quickdocs.org/clack/)](#with-clackhttpquickdocsorgclack)
        - [with Docker](#with-docker)
        - [On Heroku](#on-heroku)
        - [Daemonizing, restarting in case of crashes, handling logs](#daemonizing-restarting-in-case-of-crashes-handling-logs)
        - [Debugging SBCL error: ensure_space: failed to allocate n bytes](#debugging-sbcl-error-ensurespace-failed-to-allocate-n-bytes)
    - [Connecting to a remote Swank server](#connecting-to-a-remote-swank-server)
    - [Hot reload](#hot-reload)
- [Appendice I: Example websites built with Lisp:](#appendice-i-example-websites-built-with-lisp)
- [Appendice II: Example software](#appendice-ii-example-software)

<!-- markdown-toc end -->


# Web application environments

## Clack, Lack

# Web frameworks

## Hunchentoot

The de-facto web server, with the best documentation (cough cough),
the most websites on production. Lower level than a web framework
(defining routes seems weird at first). I think worth knowing.

https://edicl.github.io/hunchentoot/

## Caveman

A popular web framework, written by a super-productive lisper, nice documentation for basic
stuff but lacking for the rest, based on Clack (webserver interface,
think Python's WSGI), uses Hunchentoot by default.

I feel like basic functions are too cumbersome (accessing url parameters).

https://github.com/fukamachi/caveman

## Lucerne

By another great lisper who's busy in other areas.

https://github.com/eudoxia0/lucerne

(staling as of writing)

## Snooze

By the maintainer of Sly, Emacs' Yasnippet,…

Defining routes is like defining functions. Built-in features that are
available as extensions in Clack-based frameworks (setting to get a
stacktrace on the browser, to fire up the debugger or to return a
404,…). Definitely worth exploring.

https://github.com/joaotavora/snooze

## Radiance

[Radiance](https://github.com/Shirakumo/radiance), with extensive tutorial and existing apps.

## cl-rest-server

[cl-rest-server](https://github.com/mmontone/cl-rest-server)

> a library for writing REST Web APIs in Common Lisp.

Features: validation via schemas, Swagger support, authentication, logging, caching, permission checking…

Seems complete. Didn't try.

## Wookie

https://github.com/orthecreedence/wookie

Asynchronous web server, by an impressive lisper, who built many async
libraries. Used for the [Turtl](https://github.com/turtl/api/) api
backend. Dealing with async brings its own set of problems (how will
be debugging ?).

Nice api to build routes, good documentation: http://wookie.lyonbros.com/


## Weblocks (solving the Javascript problem)

See our presentation below.

http://40ants.com/weblocks/quickstart.html

# Tasks

## Accessing url parameters

[Lucerne](http://borretti.me/lucerne/docs/overview.html) has a nice
`with-params` macro that makes accessing post or url query parameters a breeze:

~~~lisp
@route app (:post "/tweet")
(defview tweet ()
  (if (lucerne-auth:logged-in-p)
      (let ((user (current-user)))
        (with-params (tweet)
          (utweet.models:tweet user tweet))
        (redirect "/"))
      (render-template (+index+)
                       :error "You are not logged in.")))
~~~

---

[Snooze](https://github.com/joaotavora/snooze)'s way is simple and
lispy: we define routes like methods and parameters as keys:

~~~lisp
(defroute lispdoc (:get :text/* name &key (package :cl) (doctype 'function))
   ...
~~~

matches `/lispdoc`, `/lispdoc/foo` and `/lispdoc/foo?package=arg`.

---

On the contrary, I find Caveman's and Ningle's ways cumbersome.

Ningle:

~~~lisp
(setf (ningle:route *app* "/hello/:name")
      #'(lambda (params)
          (format nil "Hello, ~A" (cdr (assoc "name" params :test #'string=)))))
~~~

> The above controller will be invoked when you access to "/hello/Eitaro" or "/hello/Tomohiro", and then (cdr (assoc "name" params :test #'string=)) will be "Eitaro" and "Tomohiro".

and it doesn't say about query parameters. I had to [ask](https://stackoverflow.com/questions/43778570/how-to-get-url-query-parameters-in-clack-lucerne-or-caveman):

~~~lisp
(assoc "the-query-param" (clack.request:query-parameter lucerne:*request*) :test 'string=)
~~~

Caveman:

> Parameter keys contain square brackets ("[" & "]") will be parsed as structured parameters. You can access the parsed parameters as _parsed in routers.

~~~lisp
(defroute "/edit" (&key _parsed)
  (format nil "~S" (cdr (assoc "person" _parsed :test #'string=))))
;=> "((\"name\" . \"Eitaro\") (\"email\" . \"e.arrows@gmail.com\") (\"birth\" . ((\"year\" . 2000) (\"month\" . 1) (\"day\" . 1))))"
~~~


## Session an cookies

## Data storage

## SQL

[Mito](https://github.com/fukamachi/mito) works for MySQL, Postgres
and SQLite3 on SBCL and CCL.

https://lispcookbook.github.io/cl-cookbook/databases.html

We can define models with a regular class which has a `mito:dao-table-class` `:metaclass`:

~~~lisp
(defclass user ()
  ((name :col-type (:varchar 64)
         :initarg :name
         :accessor user-name)
   (email :col-type (:varchar 128)
          :initarg :email
          :accessor user-email))
  (:metaclass mito:dao-table-class)
  (:unique-keys email))
~~~

We create the table with `ensure-table-exists`:

~~~lisp
(ensure-table-exists 'user)
;-> ;; CREATE TABLE IF NOT EXISTS "user" (
;       "id" BIGSERIAL NOT NULL PRIMARY KEY,
;       "name" VARCHAR(64) NOT NULL,
;       "email" VARCHAR(128),
;       "created_at" TIMESTAMP,
;       "updated_at" TIMESTAMP
;   ) () [0 rows] | MITO.DAO:ENSURE-TABLE-EXISTS
~~~


### Persistent datastores

### Migrations

[Mito](https://github.com/fukamachi/mito) has migrations support and
DB schema versioning for MySQL, Postgres and SQLite3, on SBCL and
CCL. Once we have changed our model definition, we have commands to
see the generated SQL and to apply the migration.


We inspect the SQL: (suppose we just added the email field into the `user` class above)

~~~lisp
(mito:migration-expressions 'user)
;=> (#<SXQL-STATEMENT: ALTER TABLE user ALTER COLUMN email TYPE character varying(128), ALTER COLUMN email SET NOT NULL>
;    #<SXQL-STATEMENT: CREATE UNIQUE INDEX unique_user_email ON user (email)>)
~~~

and we can apply the migration:

~~~lisp
(mito:migrate-table 'user)
;-> ;; ALTER TABLE "user" ALTER COLUMN "email" TYPE character varying(128), ALTER COLUMN "email" SET NOT NULL () [0 rows] | MITO.MIGRATION.TABLE:MIGRATE-TABLE
;   ;; CREATE UNIQUE INDEX "unique_user_email" ON "user" ("email") () [0 rows] | MITO.MIGRATION.TABLE:MIGRATE-TABLE
;-> (#<SXQL-STATEMENT: ALTER TABLE user ALTER COLUMN email TYPE character varying(128), ALTER COLUMN email SET NOT NULL>
;    #<SXQL-STATEMENT: CREATE UNIQUE INDEX unique_user_email ON user (email)>)
~~~


[Crane](https://github.com/eudoxia0/crane) advertises **automatic**
migrations, i.e. it would run them after a `C-c C-c`. Unfortunately Crane has
some issues, it doesn't work with sqlite yet and the author is busy
elsewhere. It didn't work for me at first try.

Let's hope the author comes back to work on this in a near future.


## Forms

### Form validation

## Debugging

On an error we are usually dropped into the interactive debugger by default.

Snooze gives options:

* use the debugger,
* print the stacktrace in the browser (like clack-errors below, but built-in),
* display a custom 404.


[clack-errors](https://github.com/eudoxia0/clack-errors). Like a Flask
or Django stacktrace in the browser. For Caveman, Ningle and family.

> By default, when Clack throws an exception when rendering a page, the server waits for the response until it times out while the exception waits in the REPL. This isn't very useful. So now there's this.

It prints the stacktrace along with some request details on the
browser. Can return a custom error page in production.

<img src="https://camo.githubusercontent.com/17dd6e0a7a916c8118f0134a94404f6757bee9dc/68747470733a2f2f7261772e6769746875622e636f6d2f6575646f786961302f636c61636b2d6572726f72732f6d61737465722f73637265656e73686f742d6465762e706e67" width="800px"></img>


[clack-pretend](https://github.com/BnMcGn/clack-pretend)

> Are you tired of jumping to your web browser every time you need to test your work in Clack? Clack-pretend will capture and replay calls to your clack middleware stack. When developing a web application with clack, you will often find it inconvenient to run your code from the lisp REPL because it expects a clack environment, including perhaps, cookies or a logged-in user. With clack-pretend, you can run prior web requests from your REPL, moving development back where it belongs.


## Testing

Testing with a local DB: example of a [testing macro](https://lispcookbook.github.io/cl-cookbook/databases.html#testing).

We would use [envy](https://github.com/fukamachi/envy) to switch configurations.


## Misc

###  Oauth, Job queues, etc

# Templating engines

## HTML-based

**[Djula](https://mmontone.github.io/djula/)**: as Django
templates. Good documentation. Comes by default in Lucerne and Caveman.

We also use a dot to access attributes of dict-like variables (plists,
alists, hash-tables, arrays and CLOS objects), such a feature being
backed by the [access](https://github.com/AccelerationNet/access)
library.

We wanted once to use structs and didn't find how to it directly in
Djula, so we resorted in a quick helper function to transform the
struct in an alist.

**[Eco](https://github.com/eudoxia0/eco)** - a mix of html with lisp expressions.

Truncated example:

~~~
<body>
      <% if posts %>
        <h1>Recent Posts</h1>
        <ul id="post-list">
          <% loop for (title . snippet) in posts %>
            <li><%= title %> - <%= snippet %></li>
          <% end %>
        </ul>
        ...
~~~

## Lisp-based

I prefer the semantics of
[Spinneret](https://github.com/ruricolist/spinneret) over cl-who. It
also has more features (like embeddable markdown, warns on malformed html, and more).

# Javascript

## Parenscript

> Parenscript is a translator from an extended subset of Common Lisp to JavaScript. Parenscript code can run almost identically on both the browser (as JavaScript) and server (as Common Lisp).
> Parenscript code is treated the same way as Common Lisp code, making the full power of Lisp macros available for JavaScript. This provides a web development environment that is unmatched in its ability to reduce code duplication and provide advanced meta-programming facilities to web developers.

https://common-lisp.net/project/parenscript/

## JSCL

> A Lisp-to-Javascript compiler bootstrapped from Common Lisp and executed from the browser.

https://github.com/jscl-project/jscl

https://t-cool.github.io/jscl-playground/

## Ajax


## The case Webblocks - Reblocks, 2017

Weblocks is an "isomorphic" web frameworks that solves the "Javascript
problem". It allows to write the backend and an interactive client
interface in Lisp, without a line of Javascript, in our usual Lisp
development environment.

The framework evolves around widgets, that are updated server-side
and are automatically redisplayed with transparent ajax calls on the
client.

It is being massively refactored, simplified, rewritten and documented
since 2017. See the new quickstart:

http://40ants.com/weblocks/quickstart.html

Writing a dynamic todo-app resolves in:

- defining a widget class for a task:

~~~lisp
(defwidget task ()
        ((title
          :initarg :title
          :accessor title)
         (done
          :initarg :done
          :initform nil
          :accessor done)))
~~~

- doing the same for a list of tasks:

~~~lisp
(defwidget task-list ()
        ((tasks
          :initarg :tasks
          :accessor tasks)))
~~~

- saying how to render these widgets in html by extending the `render` method:

~~~lisp
(defmethod render ((task task))
        "Render a task."
        (with-html
              (:span (if (done task)
                         (with-html
                               (:s (title task)))
                       (title task)))))

(defmethod render ((widget task-list))
        "Render a list of tasks."
        (with-html
              (:h1 "Tasks")
              (:ul
                (loop for task in (tasks widget) do
                      (:li (render task))))))
~~~

- telling how to initialize the Weblocks app:

~~~lisp
(defmethod weblocks/session:init ((app tasks))
         (declare (ignorable app))
         (let ((tasks (make-task-list "Make my first Weblocks app"
                                      "Deploy it somewhere"
                                      "Have a profit")))
           (make-instance 'task-list :tasks tasks)))
~~~

- and then writing functions to interact with the widgets, for example adding a task:

~~~lisp
(defmethod add-task ((task-list task-list) title)
        (push (make-task title)
              (tasks task-list))
        (update task-list))
~~~

Adding an html form and calling the new `add-task` function:

~~~lisp
(defmethod render ((task-list task-list))
        (with-html
          (:h1 "Tasks")
          (loop for task in (tasks task-list) do
            (render task))
          (with-html-form (:POST (lambda (&key title &allow-other-keys)
                                         (add-task task-list title)))
            (:input :type "text"
                    :name "title"
                    :placeholder "Task's title")
            (:input :type "submit"
                    :value "Add"))))
~~~


# Shipping

## Building

We can build an executable also for web apps. That makes for a simple deployment process.

This is the general way:

~~~lisp
(sb-ext:save-lisp-and-die #p"name-of-executable" :toplevel #'main :executable t)
~~~

we need a step more for web apps:

~~~lisp
(defun main ()
    ;; with bordeaux-threads. Also sb-ext: join-thread, thread-name, list-all-threads.
    (bt:join-thread (find-if (lambda (th)
                                (search "hunchentoot" (bt:thread-name th)))
                              (bt:all-threads))))
~~~

I can now build my web app, send it to my VPS and see it live.

When I run it, Hunchentoot stays listening at the foreground:

```
$ ./my-webapp
Hunchentoot server is started.
Listening on localhost:9003.
```

I need to put it in the background (`C-z bg`), or use a `tmux` session
(`tmux`, then `C-b d` to detach it).

To be complete, you'll notice that we can not `C-c` our running app,
we get trapped into the debugger (which responds only to `C-z` and
`kill`). As with any command line, we have to catch the corresponding
signal. We also `stop` our app. See
[our cl-torrents tutorial](https://vindarel.github.io/cl-torrents/tutorial.html#org8567d07)
on how to build command-line applications.

~~~lisp
(defun main ()
  (start-app :port 9003)
  ;; with bordeaux-threads
  (handler-case (bt:join-thread (find-if (lambda (th)
                                             (search "hunchentoot" (bt:thread-name th)))
                                         (bt:all-threads)))
    (#+sbcl sb-sys:interactive-interrupt
      #+ccl  ccl:interrupt-signal-condition
      #+clisp system::simple-interrupt-condition
      #+ecl ext:interactive-interrupt
      #+allegro excl:interrupt-signal
      () (progn
           (format *error-output* "Aborting.~&")
           (clack:stop *server*)
           (uiop:quit 1)) ;; portable exit, included in ASDF, already loaded.
    ;; for others, unhandled errors (we might want to do the same).
    (error (c) (format t "Woops, an unknown error occured:~&~a~&" c)))))
~~~


See also how to daemonize an application (below in Deployment).


To see:

- a Debian package for every Quicklisp system: [http://margaine.com/2015/12/22/quicklisp-packagecloud-debian-packages.html](http://margaine.com/2015/12/22/quicklisp-packagecloud-debian-packages.html).


## Multiplatform delivery with Electron (Ceramic)

[Ceramic](https://ceramic.github.io/) makes all the work for us.

It is as simple as this:

~~~lisp
;; Load Ceramic and our app
(ql:quickload '(:ceramic :our-app))

;; Ensure Ceramic is set up
(ceramic:setup)
(ceramic:interactive)

;; Start our app (here based on the Lucerne framework)
(lucerne:start our-app.views:app :port 8000)

;; Open a browser window to it
(defvar window (ceramic:make-window :url "http://localhost:8000/"))

;; start Ceramic
(ceramic:show-window window)
~~~

and we can ship this on Linux, Mac and Windows.

More:

> Ceramic applications are compiled down to native code, ensuring both performance and enabling you to deliver closed-source, commercial applications.

(so no need to minify our JS)

with one more line:

~~~lisp
(ceramic.bundler:bundle :ceramic-hello-world
                                 :bundle-pathname #p"/home/me/app.tar")
Copying resources...
Compiling app...
Compressing...
Done!
#P"/home/me/app.tar"
~~~

This last line was buggy for us.

# Deployment

Radiance's tutorial talks about deployment. https://github.com/Shirakumo/radiance-tutorial/blob/master/Part%207.md

## Running the app on a web server

### Manually

    sbcl --load <my-app> --eval (start-my-app)

For example, a `run` Makefile target:

```
run:
	sbcl --load my-app.asd \
	     --eval '(ql:quickload :my-app)' \
	     --eval '(my-app:start-app)'  ;; given this function starts clack or hunchentoot.
```

this keeps sbcl in the foreground. Can use `tmux` or just `C-z bg` to put it in background.

Then, we need of a task supervisor, that will restart our app on failures, start it after a reboot, handle logging. See the section below and example projects (such as Quickutil).


### with [Clack](http://quickdocs.org/clack/)

```
$ clackup app.lisp
Hunchentoot server is started.
Listening on localhost:5000.
```

### with Docker

So we have various implementations ready to use: sbcl, ecl, ccl… with Quicklisp well configured.

https://lispcookbook.github.io/cl-cookbook/testing.html#gitlab-ci


### On Heroku

See [heroku-buildpack-common-lisp](https://gitlab.com/duncan-bayne/heroku-buildpack-common-lisp) and the [Awesome CL#deploy](https://github.com/CodyReichert/awesome-cl#deployment) section.


### Daemonizing, restarting in case of crashes, handling logs (Systemd)

Generally, this depends on your system. But most GNU/Linux distros now come with Systemd. Write a service file like this:

```
$ /etc/systemd/system/my-app.service
[Unit]
Description=stupid simple example

[Service]
WorkingDirectory=/path/to/your/app
ExecStart=sbcl --load run.lisp  # your command
Type=simple
Restart=always
RestartSec=10
```

run a command to start it:

    sudo systemctl start my-app.service

a command to check its status:

    systemctl status my-app.service


Systemd handles **logging**. We write to stdout or stderr, it writes logs:

    journalctl -f -u my-app.service


and it handles crashes and **restarts the app**:

    Restart=always

and it can **start the app after a reboot**:

    [Install]
    WantedBy=basic.target

to enable it:

    sudo systemctl enable my-app.service


### Debugging SBCL error: ensure_space: failed to allocate n bytes

If you get this error with SBCL on your server:

```
mmap: wanted 1040384 bytes at 0x20000000, actually mapped at 0x715fa2145000
ensure_space: failed to allocate 1040384 bytes at 0x20000000
(hint: Try "ulimit -a"; maybe you should increase memory limits.)
```

then disable [ASLR](https://en.wikipedia.org/wiki/Address_space_layout_randomization):

    sudo bash -c "echo 0 > /proc/sys/kernel/randomize_va_space"

## Connecting to a remote Swank server

Little example here: [http://cvberry.com/tech_writings/howtos/remotely_modifying_a_running_program_using_swank.html](http://cvberry.com/tech_writings/howtos/remotely_modifying_a_running_program_using_swank.html).

It defines a simple function that prints forever:


~~~lisp
;; a little common lisp swank demo
;; while this program is running, you can connect to it from another terminal or machine
;; and change the definition of doprint to print something else out!
;; (ql:quickload '(:swank :bordeaux-threads))

(require :swank)
(require :bordeaux-threads)

(defparameter *counter* 0)

(defun dostuff ()
  (format t "hello world ~a!~%" *counter*))

(defun runner ()
  (bt:make-thread (lambda ()
                    (swank:create-server :port 4006)))
  (format t "we are past go!~%")
  (loop while t do
       (sleep 5)
       (dostuff)
       (incf *counter*)))

(runner)
~~~

On our server, we run it with

    sbcl --load demo.lisp

we do port forwarding on our development machine:

    ssh -L4006:127.0.0.1:4006 username@example.com

this will securely forward port 4006 on the server at example.com to
our local computer's port 4006 (swanks accepts connections from
localhost).

We connect to the running swank with `M-x slime-connect`, typing in
port 4006.

We can write new code:

~~~lisp
(defun dostuff ()
  (format t "goodbye world ~a!~%" *counter*))
(setf *counter* 0)
~~~

and eval it as usual with `M-x slime-eval-region` for instance. The output should change.

There are more pointers on CV Berry's page.


## Hot reload

Example with [Quickutil](https://github.com/tarballs-are-good/quickutil/blob/master/quickutil-server/).

It has a Makefile target:

```lisp
hot_deploy:
	$(call $(LISP), \
		(ql:quickload :quickutil-server) (ql:quickload :swank-client), \
		(swank-client:with-slime-connection (conn "localhost" $(SWANK_PORT)) \
			(swank-client:slime-eval (quote (handler-bind ((error (function continue))) \
				(ql:quickload :quickutil-utilities) (ql:quickload :quickutil-server) \
				(funcall (symbol-function (intern "STOP" :quickutil-server))) \
				(funcall (symbol-function (intern "START" :quickutil-server)) $(start_args)))) conn)) \
		$($(LISP)-quit))
```

It has to be run on the server (a simple fabfile command can call this
through ssh). Beforehand, a `fab update` has run `git pull` on the
server, so new code is present but not running. It connects to the
local swank server, loads the new code, stops and starts the app in a
row.
