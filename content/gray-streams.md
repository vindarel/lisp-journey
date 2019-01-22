+++
date = "2019-01-22T07:51:49+01:00"
title = "Gray streams"
draft = false
+++

This is a copy of http://www.nhplace.com/kent/CL/Issues/stream-definition-by-user.html with syntax highlighting.


FAILED Issue STREAM-DEFINITION-BY-USER ("Gray Streams")

This is the writeup of failed issue STREAM-DEFINITION-BY-USER. Because it did not pass, it has no official standing other than as a historical document.

NOTES:

-        Several vendors have implemented this proposal anyway, so if you'd like to use this facility, you might check to see if it's available in your implementation of choice in spite of not being part the "official" standard.

-       The facility described here is commonly referred to as "Gray Streams", after David Gray, who wrote the proposal—please do not write this as "Grey Streams"!

-       Another facility of note that came later and may be available in some implementations is Franz's "Simple Streams". It is newer and addresses a broader range of issues, but its availability in this or that implementation may be different.

[Click here to see my personal notes on this issue.](http://www.nhplace.com/kent/CL/Issues/stream-definition-by-user-notes.html)
--Kent Pitman (10-Mar-2001)


Issue:		STREAM-DEFINITION-BY-USER

References:	CLtL pages 329-332, 378-381, and 384-385.

Related issues:	STREAM-INFO, CLOSED-STREAM-FUNCTIONS, STREAM-ACCESS,
		STREAM-CAPABILITIES

Category:	ADDITION

Edit history:	Version 1, 22-Mar-89 by David N. Gray

Status:		For discussion and evaluation; not proposed for
		inclusion in the standard at this time.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [Problem description](#problem-description)
- [Proposal `stream-definition-by-user:generic-functions`](#proposal-stream-definition-by-usergeneric-functions)
    - [Overview](#overview)
    - [Character input:](#character-input)
    - [Character output](#character-output)
- [Rationale](#rationale)
- [Current practice](#current-practice)
- [Examples](#examples)
- [Cost to Implementors](#cost-to-implementors)
- [Cost to Users](#cost-to-users)
- [Cost of non-adoption](#cost-of-non-adoption)
- [Performance impact](#performance-impact)
- [Benefits](#benefits)
- [Esthetics](#esthetics)
- [Discussion](#discussion)

<!-- markdown-toc end -->


# Problem description

Common Lisp does not provide a standard way for users to define their
  own streams for use by the standard I/O functions.  This impedes the
  development of window systems for Common Lisp because, while there are
  standard Common Lisp I/O functions and there are beginning to be
  standard window systems, there is no portable way to connect them
  together to make a portable Common Lisp window system.

  There are also many applications where users might want to define
  their own filter streams for doing things like printer device control,
  report formatting, character code translation, or
  encryption/decryption.

# Proposal `stream-definition-by-user:generic-functions`

## Overview

  Define a set of generic functions for performing I/O.  These functions
  will have methods that specialize on the stream argument; they would
  be used by the existing I/O functions.  Users could write additional
  methods for them in order to support their own stream classes.

  Define a set of classes to be used as the superclass of a stream class
  in order to provide some default methods.

 ## Classes

  The following classes are to be used as super classes of user-defined
  stream classes.  They are not intended to be directly instantiated; they
  just provide places to hang default methods.

  `fundamental-stream`				[Class]

    This class is a subclass of `stream` and of `standard-object`.  `streamp`
    will return true for an instance of any class that includes this.  (It
    may return true for some other things also.)

  `fundamental-input-stream`			[Class]

    A subclass of `fundamental-stream`.  Its inclusion causes `input-stream-p`
    to return true.

  `fundamental-output-stream`			[Class]

    A subclass of `fundamental-stream`.  Its inclusion causes `output-stream-p`
    to return true.  Bi-direction streams may be formed by including both
    `fundamental-output-stream` and `fundamental-input-stream`.

  `fundamental-character-stream`			[Class]

    A subclass of `fundamental-stream`.  It provides a method for
    `stream-element-type` which returns `character`.

  `fundamental-binary-stream`			[Class]

    A subclass of `fundamental-stream`.  Any instantiable class that
    includes this needs to define a method for `stream-element-type`.

  `fundamental-character-input-stream`		[Class]

    Includes `fundamental-input-stream` and `fundamental-character-stream`.
    It provides default methods for several generic functions used for
    character input.

  `fundamental-character-output-stream`		[Class]

    Includes `fundamental-output-stream` and `fundamental-character-stream`.
    It provides default methods for several generic functions used for
    character output.

  `fundamental-binary-input-stream`		[Class]

    Includes `fundamental-input-stream` and `fundamental-binary-stream`.

  `fundamental-binary-output-stream`		[Class]

    Includes `fundamental-output-stream` and `fundamental-binary-stream`.


## Character input

  A character input stream can be created by defining a class that
  includes `fundamental-character-input-stream` and defining methods for the
  generic functions below.

  `stream-read-char`  stream			[Generic Function]

    This reads one character from the stream.  It returns either a
    character object, or the symbol :EOF if the stream is at end-of-file.
    Every subclass of `fundamental-character-input-stream` must define a
    method for this function.

    Note that for all of these generic functions, the stream argument
    must be a stream object, not T or NIL.

  `stream-unread-char`  stream  character		[Generic Function]

    Un-does the last call to `stream-read-char`, as in `unread-char`.  Returns
    NIL.  Every subclass of `fundamental-character-input-stream` must define
    a method for this function.

  `stream-read-char-no-hang`  stream		[Generic Function]

    This is used to implement `read-char-no-hang`.  It returns either a
    character, or NIL if no input is currently available, or :EOF if
    end-of-file is reached.  The default method provided by
    `fundamental-character-input-stream` simply calls `stream-read-char`; this
    is sufficient for file streams, but interactive streams should define
    their own method.

  `stream-peek-char`  stream			[Generic Function]

    Used to implement `peek-char`; this corresponds to peek-type of NIL.
    It returns either a character or :EOF.  The default method
    calls `stream-read-char` and `stream-unread-char`.

  `stream-listen`  stream				[Generic Function]

    Used by `listen`.  Returns true or false.  The default method uses
    `stream-read-char-no-hang` and `stream-unread-char`.  Most streams should
    define their own method since it will usually be trivial and will
    always be more efficient than the default method.

  `stream-read-line`  stream			[Generic Function]

    Used by `read-line`.  A string is returned as the first value.  The
    second value is true if the string was terminated by end-of-file
    instead of the end of a line.  The default method uses repeated
    calls to `stream-read-char`.

  `stream-clear-input`  stream			[Generic Function]

    Implements `clear-input` for the stream, returning NIL.  The default
    method does nothing.


## Character output

  A character output stream can be created by defining a class that
  includes `fundamental-character-output-stream` and defining methods for the
  generic functions below.

  `stream-write-char`  stream character		[Generic Function]

    Writes character to the stream and returns the character.  Every
    subclass of `fundamental-character-output-stream` must have a method
    defined for this function.

  `stream-line-column`  stream			[Generic Function]

    This function returns the column number where the next character
    will be written, or NIL if that is not meaningful for this stream.
    The first column on a line is numbered 0.  This function is used in
    the implementation of `pprint` and the FORMAT ~T directive.  For every
    character output stream class that is defined, a method must be
    defined for this function, although it is permissible for it to
    always return NIL.

  `stream-start-line-p`  stream			[Generic Function]

    This is a predicate which returns T if the stream is positioned at the
    beginning of a line, else NIL.  It is permissible to always return
    NIL.  This is used in the implementation of `fresh-line`.  Note that
    while a value of 0 from `stream-line-column` also indicates the
    beginning of a line, there are cases where `stream-start-line-p` can be
    meaningfully implemented although `stream-line-column` can't be.  For
    example, for a window using variable-width characters, the column
    number isn't very meaningful, but the beginning of the line does have
    a clear meaning.  The default method for `stream-start-line-p` on class
    `fundamental-character-output-stream` uses `stream-line-column`, so if
    that is defined to return `nil`, then a method should be provided for
    either `stream-start-line-p` or `stream-fresh-line`.

  `stream-write-string` stream string &optional start end [Generic Function]

    This is used by `write-string`.  It writes the string to the stream,
    optionally delimited by start and end, which default to 0 and NIL.
    The string argument is returned.  The default method provided by
    `fundamental-character-output-stream` uses repeated calls to
    `stream-write-char`.

  `stream-terpri`  stream				[Generic Function]

    Writes an end of line, as for `terpri`.  Returns NIL.  The default
    method does (`stream-write-char` stream #\NEWLINE).

  `stream-fresh-line`  stream			[Generic Function]

    Used by `fresh-line`.  The default method uses `stream-start-line-p` and
    `stream-terpri`.

  `stream-finish-output`  stream			[Generic Function]

    Implements `finish-output`.  The default method does nothing.

  `stream-force-output`  stream			[Generic Function]

    Implements `force-output`.  The default method does nothing.

  `stream-clear-output`  stream			[Generic Function]

    Implements `clear-output`.  The default method does nothing.

  `stream-advance-to-column`  stream column	[Generic Function]

    Writes enough blank space so that the next character will be written
    at the specified column.  Returns true if the operation is
    successful, or NIL if it is not supported for this stream.
    This is intended for use by by `pprint` and FORMAT ~T.  The default
    method uses `stream-line-column` and repeated calls to
    `stream-write-char` with a #\SPACE character; it returns NIL if
    `stream-line-column` returns NIL.


 ## Other functions

  `close`  stream &key abort			[Generic Function]

    The existing function `close` is redefined to be a generic function, but
    otherwise behaves the same.  The default method provided by class
    `fundamental-stream` sets a flag for `open-stream-p`.  The value returned
    by `close` will be as specified by the issue `closed-stream-operations`.

  `open-stream-p` stream				[Generic Function]

    This function [from proposal `stream-access]` is made generic.  A
    default method is provided by class `fundamental-stream` which returns
    true if `close` has not been called on the stream.

  `streamp`  object				[Generic Function]

  `input-stream-p`  stream			[Generic Function]

  `output-stream-p`  stream			[Generic Function]

    These three existing predicates may optionally be implemented as
    generic functions for implementations that want to permit users to
    define streams that are not `standard-object`s.  Normally, the default
    methods provided by classes `fundamental-input-stream` and
    `fundamental-output-stream` are sufficient.  Note that, for example,
    (INPUT-STREAM-P x) is not equivalent to (TYPEP x
    'FUNDAMENTAL-INPUT-STREAM) because implementations may have
    additional ways of defining their own streams even if they don't
    make that visible by making these predicates generic.

  `stream-element-type`  stream			[Generic Function]

    This existing function is made generic, but otherwise behaves the
    same.  Class `fundamental-character-stream` provides a default method
    which returns `character`.

  `pathname` and `truename` are also permitted to be implemented as generic
  functions.  There is no default method since these are not valid for
  all streams.


 ## Binary streams:

    Binary streams can be created by defining a class that includes either
    `fundamental-binary-input-stream` or `fundamental-binary-output-stream`
    (or both) and defining a method for `stream-element-type` and for one or
    both of the following generic functions.

  `stream-read-byte`  stream			[Generic Function]

    Used by `read-byte`; returns either an integer, or the symbol :EOF if the
    stream is at end-of-file.

  `stream-write-byte` stream integer		[Generic Function]

    Implements `write-byte`; writes the integer to the stream and returns
    the integer as the result.


# Rationale

  The existing I/O functions cannot be made generic because, in nearly
  every case, the stream argument is optional, and therefore cannot be
  specialized.  Therefore, it is necessary to define a lower-level
  generic function to be used by the existing function.  It also isn't
  appropriate to specialize on the second argument of `print-object` because
  it is a higher-level function -- even when the first argument is a
  character or a string, it needs to format it in accordance with
  `*PRINT-ESCAPE*`.

  In order to make the meaning as obvious as possible, the names of the
  generic functions have been formed by prefixing "`stream-`" to the
  corresponding non-generic function.

  Having the generic input functions just return :EOF at end-of-file, with
  the higher-level functions handling the eof-error-p and eof-value
  arguments, simplifies the generic function interface and makes it more
  efficient by not needing to pass through those arguments.  Note that the
  functions that use this convention can only return a character or
  integer as a stream element, so there is no possibility of ambiguity.

  Functions `stream-line-column`, `stream-start-line-p`, and
  `stream-advance-to-column` may appear to be a reincarnation of the
  defeated proposal `stream-info`, but the motivation here is different.
  This interface needs to be defined if user-defined streams are to be
  able to be used by `pprint` and FORMAT ~T, which could be viewed as a
  separate question from whether the user can call then on
  system-defined streams.

# Current practice

  No one currently supports exactly this proposal, but this is very
  similar to the stream interface used in `clue`.

  On descendants of the MIT Lisp Machine, streams can be implemented
  by users as either flavors, with methods to accept the various
  messages corresponding to the I/O operations, or as functions, which
  take a message keyword as their first argument.

# Examples

~~~lisp
  ;;;; Here is an example of how the default methods could be
  ;;;; implemented (omitting the most trivial ones):

  (defmethod STREAM-PEEK-CHAR ((stream fundamental-character-input-stream))
    (let ((character (stream-read-char stream)))
      (unless (eq character :eof)
	(stream-unread-char stream character))
      character))

  (defmethod STREAM-LISTEN ((stream fundamental-character-input-stream))
    (let ((char (stream-read-char-no-hang stream)))
      (and (not (null char))
	   (not (eq char :eof))
	   (progn (stream-unread-char stream char) t))))

  (defmethod STREAM-READ-LINE ((stream fundamental-character-input-stream))
    (let ((line (make-array 64 :element-type 'string-char
			    :fill-pointer 0 :adjustable t)))
      (loop (let ((character (stream-read-char stream)))
	      (if (eq character :eof)
		  (return (values line t))
		(if (eql character #\newline)
		    (return (values line nil))
		  (vector-push-extend character line)))))))

  (defmethod STREAM-START-LINE-P ((stream fundamental-character-output-stream))
    (equal (stream-line-column stream) 0))

  (defmethod STREAM-WRITE-STRING ((stream fundamental-character-output-stream)
				  string &optional (start 0)
				  (end (length string)))
    (do ((i start (1+ i)))
	((>= i end) string)
      (stream-write-char stream (char string i))))

  (defmethod STREAM-TERPRI ((stream fundamental-character-output-stream))
    (stream-write-char stream #\newline)
    nil)

  (defmethod STREAM-FRESH-LINE ((stream fundamental-character-output-stream))
    (if (stream-start-line-p stream)
	nil
      (progn (stream-terpri stream) t)))

  (defmethod STREAM-ADVANCE-TO-COLUMN ((stream fundamental-character-output-stream)
				       column)
    (let ((current (stream-line-column stream)))
      (unless (null current)
	(dotimes (i (- current column) t)
	  (stream-write-char stream #\space)))))

  (defmethod INPUT-STREAM-P ((stream fundamental-input-stream)) t)
  (defmethod INPUT-STREAM-P ((stream fundamental-output-stream))
    ;; allow the two classes to be mixed in either order
    (typep stream 'fundamental-input-stream))
  (defmethod OUTPUT-STREAM-P ((stream fundamental-output-stream)) t)
  (defmethod OUTPUT-STREAM-P ((stream fundamental-input-stream))
    (typep stream 'fundamental-output-stream))

  ;;;; Following is an example of how the existing I/O functions could
  ;;;; be implemented using standard Common Lisp and the generic
  ;;;; functions specified above.  The standard functions being defined
  ;;;; are in upper case.

  ;;  Internal helper functions

  (proclaim '(inline decode-read-arg decode-print-arg check-for-eof))
  (defun decode-read-arg (arg)
    (cond ((null arg) *standard-input*)
	  ((eq arg t) *terminal-io*)
	  (t arg)))

  (defun decode-print-arg (arg)
    (cond ((null arg) *standard-output*)
	  ((eq arg t) *terminal-io*)
	  (t arg)))

  (defun check-for-eof (value stream eof-errorp eof-value)
    (if (eq value :eof)
	(report-eof stream eof-errorp eof-value)
      value))

  (defun report-eof (stream eof-errorp eof-value)
    (if eof-errorp
	(error 'end-of-file :stream stream)
      eof-value))

  ;;;  Common Lisp input functions

  (defun READ-CHAR (&optional input-stream (eof-errorp t) eof-value recursive-p)
    (declare (ignore recursive-p)) ; a mistake in CLtL?
    (let ((stream (decode-read-arg input-stream)))
      (check-for-eof (stream-read-char stream) stream eof-errorp eof-value)))

  (defun PEEK-CHAR (&optional peek-type input-stream (eof-errorp t)
			eof-value recursive-p)
    (declare (ignore recursive-p))
    (let ((stream (decode-read-arg input-stream)))
      (if (null peek-type)
	  (check-for-eof (stream-peek-char stream) stream eof-errorp eof-value)
        (loop
	  (let ((value (stream-peek-char stream)))
	    (if (eq value :eof)
		(return (report-eof stream eof-errorp eof-value))
	      (if (if (eq peek-type t)
		      (not (member value '(#\space #\tab #\newline
					   #\page #\return #\linefeed)))
		    (char= peek-type value))
		  (return value)
		(stream-read-char stream))))))))

  (defun UNREAD-CHAR (character &optional input-stream)
    (stream-unread-char (decode-read-arg input-stream) character))

  (defun LISTEN (&optional input-stream)
    (stream-listen (decode-read-arg input-stream)))

  (defun READ-LINE (&optional input-stream (eof-error-p t)
			eof-value recursive-p)
    (declare (ignore recursive-p))
    (let ((stream (decode-read-arg input-stream)))
      (multiple-value-bind (string eofp)
	  (stream-read-line stream)
	(if eofp
	    (if (= (length string) 0)
		(report-eof stream eof-error-p eof-value)
	      (values string t))
	  (values string nil)))))

  (defun CLEAR-INPUT (&optional input-stream)
    (stream-clear-input (decode-read-arg input-stream)))

  (defun READ-CHAR-NO-HANG (&optional input-stream (eof-errorp t)
				eof-value recursive-p)
    (declare (ignore recursive-p))
    (let ((stream (decode-read-arg input-stream)))
      (check-for-eof (stream-read-char-no-hang stream)
		     stream eof-errorp eof-value)))

  ;;;  Common Lisp output functions

  (defun WRITE-CHAR (character &optional output-stream)
     (stream-write-char (decode-print-arg output-stream) character))

  (defun FRESH-LINE (&optional output-stream)
    (stream-fresh-line (decode-print-arg output-stream)))

  (defun TERPRI (&optional output-stream)
    (stream-terpri (decode-print-arg output-stream)))

  (defun WRITE-STRING (string &optional output-stream &key (start 0) end)
    (stream-write-string (decode-print-arg output-stream) string start end))

  (defun WRITE-LINE (string &optional output-stream &key (start 0) end)
    (let ((stream (decode-print-arg output-stream)))
      (stream-write-string stream string start end)
      (stream-terpri stream)
      string))

  (defun FORCE-OUTPUT (&optional stream)
    (stream-force-output (decode-print-arg stream)))

  (defun FINISH-OUTPUT (&optional stream)
    (stream-finish-output (decode-print-arg stream)))

  (defun CLEAR-OUTPUT (&optional stream)
    (stream-clear-output (decode-print-arg stream)))

  ;;;  Binary streams

  (defun READ-BYTE (binary-input-stream &optional (eof-errorp t) eof-value)
    (check-for-eof (stream-read-byte binary-input-stream)
		   binary-input-stream eof-errorp eof-value))

  (defun WRITE-BYTE (integer binary-output-stream)
    (stream-write-byte binary-output-stream integer))

  ;;;  String streams

  (defclass string-input-stream (fundamental-character-input-stream)
    ((string :initarg :string :type string)
     (index :initarg :start :type fixnum)
     (end :initarg :end :type fixnum)
     ))

  (defun MAKE-STRING-INPUT-STREAM (string &optional (start 0) end)
    (make-instance 'string-input-stream :string string
		   :start start :end (or end (length string))))

  (defmethod stream-read-char ((stream string-input-stream))
    (with-slots (index end string) stream
      (if (>= index end)
	  :eof
	(prog1 (char string index)
	       (incf index)))))

  (defmethod stream-unread-char ((stream string-input-stream) character)
    (with-slots (index end string) stream
      (decf index)
      (assert (eql (char string index) character))
      nil))

  (defmethod stream-read-line ((stream string-input-stream))
    (with-slots (index end string) stream
      (let* ((endline (position #\newline string :start index :end end))
	     (line (subseq string index endline)))
	(if endline
	    (progn (setq index (1+ endline))
		   (values line nil))
	  (progn (setq index end)
		 (values line t))))))

  (defclass string-output-stream (fundamental-character-output-stream)
    ((string :initform nil :initarg :string)))

  (defun MAKE-STRING-OUTPUT-STREAM ()
    (make-instance 'string-output-stream))

  (defun GET-OUTPUT-STREAM-STRING (stream)
    (with-slots (string) stream
      (if (null string)
	  ""
	(prog1 string (setq string nil)))))

  (defmethod stream-write-char ((stream string-output-stream) character)
    (with-slots (string) stream
      (when (null string)
	(setq string (make-array 64. :element-type 'string-char
				 :fill-pointer 0 :adjustable t)))
      (vector-push-extend character string)
      character))

  (defmethod stream-line-column ((stream string-output-stream))
    (with-slots (string) stream
      (if (null string)
	  0
	(let ((nx (position #\newline string :from-end t)))
	  (if (null nx)
	      (length string)
	    (- (length string) nx 1))
	  ))))
~~~

# Cost to Implementors

  Given that CLOS is supported, adding the above generic functions and
  methods is easy, since most of the code is included in the examples
  above.  The hard part would be re-writing existing I/O functionality in
  terms of methods on these new generic functions.  That could be
  simplified if methods can be defined to forward the operations to the
  old representation of streams.  For a new implementation, the cost could
  be zero since an approach similar to this would likely be used anyway.

# Cost to Users

  None; this is an upward-compatible addition.   Users won't even
  need to know anything about this unless they actually need this feature.

# Cost of non-adoption

  Development of portable I/O extensions will be discouraged.

# Performance impact

  This shouldn't affect performance of new implementations (assuming an
  efficient CLOS implementation), but it could slow down I/O if it were
  clumsily grafted on top of an existing implementation.

# Benefits

  A broader domain of programs that can be written portably.

# Esthetics

  This seems to be a simple, straight-forward approach.

# Discussion

  This proposal incorporates suggestions made by several people in
  response to an earlier outline.  So far, no one has expressed opposition
  to the concept.  There are some differences of opinion about whether
  certain operations should have default methods or required methods:
  `stream-listen`, `stream-read-char-no-hang`, `stream-line-column`,
  and `stream-start-line-p`.

  An experimental prototype of this has been successfully implemented on
  the Explorer.

  This proposal does not provide sufficient capability to implement
  forwarding streams such as for `make-synonym-stream`,
  `make-broadcast-stream`, `make-concatenated-stream`, `make-two-way-stream`, or
  `make-echo-stream`.  The generic function approach does not lend itself as
  well to that as a message passing model where the intermediary does not
  need to know what all the possible messages are.  A possible way of
  extending it for that would be to define a class

~~~lisp
    (defclass stream-generic-function (standard-generic-function) ())
~~~

  to be used as the :generic-function-class option for all of the I/O
  generic functions.  This would then permit doing something like

~~~lisp
  (defmethod no-applicable-method ((gfun stream-generic-function) &rest args)
    (if (streamp (first args))
	(apply #'stream-operation-not-handled (first args) gfun (rest args))
      (call-next-method)))
~~~

  where stream-operation-not-handled is a generic function whose default
  method signals an error, but forwarding streams can define methods that
  will create a method to handle the unexpected operation.  (Perhaps
  `no-applicable-method` should be changed to take two required arguments
  since all generic functions need at least one required argument, and
  that would make it unnecessary to define a new generic function class
  just to be able to write this one method.)

  Another thing that is not addressed here is a way to cause an instance
  of a user-defined stream class to be created from a call to the `open`
  function.  That should be part of a separate issue for generic functions
  on pathnames.  If that capability were available, then `pathname` and
  `truename` should be required to be generic functions.

  An earlier draft defined just two classes, `fundamental-input-stream` and
  `fundamental-output-stream`, that were used for both character and binary
  streams.  It isn't clear whether that simple approach is sufficient or
  whether the larger set of classes is really needed.
