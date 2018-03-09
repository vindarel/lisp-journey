+++
date = "2018-01-23T07:51:49+01:00"
title = "(bookmark) Get a list of all the dependencies of a lisp system"
draft = false
+++

I'll save here
[a reddit discussion](https://www.reddit.com/r/Common_Lisp/comments/82wiyt/how_to_collect_all_asdf_dependencies_for/),
which I find interesting but that will be burried quickly down
reddit's history. The goal is to **get all the dependencies of a
system**.

You'd better read the OP's question and the discussion (where the OP
is the experimented [svetlyak40wt/40ants](https://github.com/40ants/),
at the moment doing a god's work on Weblocks).

His solution is https://gist.github.com/svetlyak40wt/03bc68c820bb3e45bc7871870379c42e

```lisp
(ql:quickload :fset)

(defun get-dependencies (system)
  "Returns a set with all dependencies of a given system.
   System should be loaded first."
  (labels ((normalize (name)
             (etypecase name
               (string (string-downcase name))
               (symbol (normalize (symbol-name name)))
               (list
                (let ((dep-type (first name))
                      (supported-dep-types (list :version :feature :require)))
                  (unless (member dep-type
                                  supported-dep-types)
                    (error "This component \"~A\" should have first element from this list: ~A."
                           name
                           supported-dep-types))

                  (normalize
                   (case dep-type
                     (:version (second name))
                     (:feature (third name))
                     (:require (second name)))))))))

    (let ((processed (fset:set))
          (queue (fset:set (normalize system))))

      (do ((current-name (fset:arb queue)
                         (fset:arb queue)))
          ((null current-name)
           ;; return result
           processed)

        ;; Remove current name from the queue
        (setf queue
              (fset:less queue current-name))
        ;; And put it into the "processed" pool
        (setf processed
              (fset:with processed current-name))

        ;; And add it's dependencies which aren't processed or in the queue already
        ;; Sometimes system can't be found because itself depends on some feature,
        ;; for example, you can specify dependency as a list:
        ;; (:FEATURE :SBCL (:REQUIRE :SB-INTROSPECT))
        ;; and it will be loaded only on SBCL.
        ;; When we are collecting dependencies on another implementation,
        ;; we don't want to fail with an error because ASDF is unable to find
        ;; such dependencies
        (let* ((system (ignore-errors
                        (asdf:find-system current-name)))
               (deps (when system
                       (asdf:component-sideway-dependencies system))))
          (dolist (dep deps)
            (let ((normalized-dep (normalize dep)))
              (unless (or (fset:lookup processed normalized-dep)
                          (fset:lookup queue normalized-dep))
                (setf queue
                      (fset:with queue normalized-dep)))))))

      (values processed))))

#|
DEPENDENCIES> (ql:quickload :clinch)
DEPENDENCIES> (get-dependencies :clinch)
#{
  "cffi"
  "sdl2"
  "uiop"
  "babel"
  "swank"
  "clinch"
  "cl-glut"
  "cl-json"
  "cl-ppcre"
  "rtg-math"
  "cl-opengl"
  "cl-plus-c"
  "alexandria"
  "cl-autowrap"
  "glsl-symbols"
  "defpackage-plus"
  "trivial-garbage"
  "trivial-timeout"
  "bordeaux-threads"
  "trivial-channels"
  "trivial-features" }
|#
```

There's also `(ql-dist:dependency-tree "mgl")` which has limitations
though, it's only for Quicklisp projects and doesn't work with
everything (see the thread).

That's all folks !
