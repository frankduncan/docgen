(defpackage #:docgen (:use :cl)
 (:export #:validate-package #:export-package #:validation-failure #:pretty-print-validate-packages)
 (:documentation "Main docgen package.

Use docgen to validate that documentation strings on external symbols adhere to
a strict format and exist, so that they can be output to markdown format, while
looking decent when used within a common lisp process."))

(defpackage #:docgen-func (:use :cl)
 (:export #:doc->ast #:ast->md #:ast->link #:ast->short-name #:ast->short-desc #:ast->category-name))

(defpackage #:docgen-pkg (:use :cl)
 (:export #:doc->ast #:ast->md))

(defpackage #:docgen-struc (:use :cl)
 (:export #:doc->ast #:ast->md #:ast->link #:ast->short-name #:ast->short-desc #:ast->category-name))
