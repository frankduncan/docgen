(defpackage #:docgen (:use :cl)
 (:export #:validate-package #:export-package #:validation-failure))

(defpackage #:docgen-func (:use :cl)
 (:export #:doc->ast #:ast->md))
