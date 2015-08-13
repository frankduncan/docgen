(in-package #:docgen-test)

(defvar *tests* nil)

; This really is just here to check against regressions
(defun run-all-tests ()
 (let
  ((results (mapcar #'funcall (reverse *tests*))))
  (every #'identity results)))

(defun slurp-file (filename &key (element-type 'character) (sequence-type 'string))
 (with-open-file (str filename :element-type element-type)
  (let ((seq (make-sequence sequence-type (file-length str)))) (read-sequence seq str) seq)))

(defmacro deftest (name f)
 `(push
   (lambda ()
    (let
     ((success
       (handler-case
        (funcall ,f)
        (error (e) (format t "Got unexpected error in tests: ~A" e)))))
     (if success
      (format t "~c[1;32m- ~A passed~c[0m~%" #\Esc ,name #\Esc)
      (format t "~c[1;31m- ~A failed~c[0m~%" #\Esc ,name #\Esc))
     success))
   *tests*))

(defmacro defsuccesstest (pkg source target)
 `(deftest
   ,source
   (lambda ()
    (handler-case
     (progn
      (load ,source)
      (string= (slurp-file ,target) (docgen:export-package ,pkg)))
     (docgen:validation-failure (vf)
      (format t "Validation failure gotten: ~A~%"
       (funcall (symbol-function (find-symbol "VALIDATION-FAILURE-MSG" :docgen)) vf)))))))

(defmacro deffailuretest (pkg source expected)
 `(deftest
   ,source
   (lambda ()
    (progn
     (load ,source)
     (let
      ((result (docgen:validate-package ,pkg)))
      (or
       (equal ,expected result)
       (format t "  Got error:~%~S~%  but expected~%~S~%" result ,expected)))))))

(defmacro deffailure-func-test (name doc expected)
 `(deftest
   ,name
   (lambda ()
    (handler-case
     (progn
      (funcall
       (symbol-function (find-symbol "INTERNAL-DOC->AST" :docgen-func))
       'unused
       ,doc)
      nil)
     (docgen:validation-failure (vf)
      (let
       ((result (funcall (symbol-function (find-symbol "VALIDATION-FAILURE-MSG" :docgen)) vf)))
       (or
        (string= ,expected result)
        (format t "  Got error:~%~S~%  but expected~%~S~%" result ,expected))))))))

(defsuccesstest :success1 "resources/success1.lisp" "resources/success1.md")
(deffailuretest :emptydocs "resources/emptydocs.lisp"
 '((:failure "Package EMPTYDOCS has no documentation")
   (:failure "Symbol NO-DOC-FUNC has no documentation")))
