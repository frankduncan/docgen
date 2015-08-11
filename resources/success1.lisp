(defpackage #:success1 (:use :cl)
 (:documentation
"This defines a simple successful package.

This is should all get pulled in and the markdown.md should be equal
to success1.md.")
 (:export
  #:func-that-does-stuff #:noargs #:result-list #:has-no-examples
  #:values-result #:has-optional #:has-keywords #:has-rest
  ))

(in-package #:success1)

(defun func-that-does-stuff (path x)
 "FUNC-THAT-DOES-STUFF PATH X => RESULT

  RESULT: SUCCESS-RESULT | FAILURE-RESULT
  SUCCESS-RESULT: (:success FILENAME)
  FAILURE-RESULT: (:failure FILENAME MSG)

ARGUMENTS AND VALUES:

  PATH: a pathname
  X: a random value related to PATH
  FILENAME: the file this func was run on
  MSG: a string containing the failure message

DESCRIPTION:

  FUNC-THAT-DOES-STUFF runs all the things against a file and returns
  as soon as the first func error is found.

  This second section uses PATH and X as something we should talk about, but
  doesn't use all the arguments (let's include PATH here for fun)

EXAMPLES:

  (func-that-does-stuff #P\"path/to/file.lisp\" t) => (:success \"path/to/file.lisp\")
  (func-that-does-stuff #P\"path/to/error.lisp\" nil) => (:failure \"path/to/error.lisp\" \"Error msg\" 20 0)"
  path)

(defun result-list ()
 "RESULT-LIST => RESULT

  RESULT: FAILURE-RESULT*
  FAILURE-RESULT: (:failure FILENAME MSG)

ARGUMENTS AND VALUES:

  FILENAME: the file this func was run on
  MSG: a string containing the failure message

DESCRIPTION:

  NOARGS runs all the things against a file and returns
  as soon as the first func error is found."
  nil)

(defun noargs ()
 "NOARGS => RESULT

  RESULT: SUCCESS-RESULT | FAILURE-RESULT
  SUCCESS-RESULT: (:success FILENAME)
  FAILURE-RESULT: (:failure FILENAME MSG)

ARGUMENTS AND VALUES:

  FILENAME: the file this func was run on
  MSG: a string containing the failure message

DESCRIPTION:

  NOARGS runs all the things against a file and returns
  as soon as the first func error is found.

EXAMPLES:

  (func-that-does-stuff) => (:success \"path/to/file.lisp\")
  (func-that-does-stuff) => (:failure \"path/to/error.lisp\" \"Error msg\" 20 0)"
  nil)

(defun has-no-examples ()
 "HAS-NO-EXAMPLES => RESULT

  RESULT: SUCCESS-RESULT | FAILURE-RESULT
  SUCCESS-RESULT: (:success FILENAME)
  FAILURE-RESULT: (:failure FILENAME MSG)

ARGUMENTS AND VALUES:

  FILENAME: the file this func was run on
  MSG: a string containing the failure message

DESCRIPTION:

  HAS-NO-EXAMPLES runs all the things against a file and returns
  as soon as the first func error is found."
  nil)

(defun values-result ()
 "VALUES-RESULT => RESULT1, RESULT2, RESULT3

  RESULT1: SUCCESS-RESULT | FAILURE-RESULT
  SUCCESS-RESULT: (:success FILENAME)
  FAILURE-RESULT: (:failure FILENAME MSG)

ARGUMENTS AND VALUES:

  RESULT2: second result
  RESULT3: third result
  FILENAME: the file this func was run on
  MSG: a string containing the failure message

DESCRIPTION:

  VALUES-RESULT runs all the things against a file and returns
  as soon as the first func error is found."
  nil)

(defun has-optional (path &optional x)
 "HAS-OPTIONAL PATH &optional X => RESULT

  RESULT: SUCCESS-RESULT | FAILURE-RESULT
  SUCCESS-RESULT: (:success FILENAME)
  FAILURE-RESULT: (:failure FILENAME MSG)

ARGUMENTS AND VALUES:

  PATH: a pathname
  X: a random value related to PATH
  FILENAME: the file this func was run on
  MSG: a string containing the failure message

DESCRIPTION:

  HAS-OPTIONAL runs all the things against a file and returns
  as soon as the first func error is found.

  This second section uses PATH and X as something we should talk about, but
  doesn't use all the arguments (let's include PATH here for fun)"
  path)

(defun has-keywords (path &key x)
 "HAS-KEYWORDS PATH &key X => RESULT

  RESULT: SUCCESS-RESULT | FAILURE-RESULT
  SUCCESS-RESULT: (:success FILENAME)
  FAILURE-RESULT: (:failure FILENAME MSG)

ARGUMENTS AND VALUES:

  PATH: a pathname
  X: a random value related to PATH
  FILENAME: the file this func was run on
  MSG: a string containing the failure message

DESCRIPTION:

  HAS-KEYWORDS runs all the things against a file and returns
  as soon as the first func error is found.

  This second section uses PATH and X as something we should talk about, but
  doesn't use all the arguments (let's include PATH here for fun)"
  path)

(defun has-rest (path &rest x)
 "HAS-REST PATH &rest X => RESULT

  RESULT: SUCCESS-RESULT | FAILURE-RESULT
  SUCCESS-RESULT: (:success FILENAME)
  FAILURE-RESULT: (:failure FILENAME MSG)

ARGUMENTS AND VALUES:

  PATH: a pathname
  X: a random value related to PATH
  FILENAME: the file this func was run on
  MSG: a string containing the failure message

DESCRIPTION:

  HAS-REST runs all the things against a file and returns
  as soon as the first func error is found.

  This second section uses PATH and X as something we should talk about, but
  doesn't use all the arguments (let's include PATH here for fun)"
  path)
