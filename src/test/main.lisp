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
     ((success (funcall ,f)))
     (if success
      (format t "~c[1;32m- ~A passed~c[0m~%" #\Esc ,name #\Esc)
      (format t "~c[1;31m- ~A failed~c[0m~%" #\Esc ,name #\Esc))
     success))
   *tests*))

(defmacro defsuccesstest (pkg source target)
 `(deftest
   ,source
   (lambda ()
    (load ,source)
    (ignore-errors (string= (slurp-file ,target) (docgen:export-package ,pkg))))))

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
