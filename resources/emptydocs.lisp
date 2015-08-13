(defpackage #:emptydocs (:use :cl)
 (:export #:no-doc-func))

(in-package #:emptydocs)

(defun no-doc-func (path x) path)
