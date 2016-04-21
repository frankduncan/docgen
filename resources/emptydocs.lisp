(defpackage #:emptydocs (:use :cl)
 (:export #:no-doc-condition #:no-doc-func))

(defvar *special-variable* nil)

(in-package #:emptydocs)

(define-condition no-doc-condition nil nil)

(defun no-doc-func (path x) path)
