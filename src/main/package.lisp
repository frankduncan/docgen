(defpackage #:docgen (:use :cl)
 (:export #:validate-package #:export-package #:validation-failure #:pretty-print-validate-packages))

(defpackage #:docgen-func (:use :cl)
 (:export #:doc->ast #:ast->md))

(defpackage #:docgen-pkg (:use :cl)
 (:export #:doc->ast #:ast->md))

(defpackage #:docgen-struc (:use :cl)
 (:export #:doc->ast #:ast->md))
