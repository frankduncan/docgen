(in-package #:docgen)

(define-condition validation-failure nil ((msg :initarg :msg :reader validation-failure-msg))
 (:documentation "Used internally for docgen parts to signal a validation error."))

(defun get-symb-type (symb)
 (cond
  ;((documentation symb 'variable) :variable)
  ((documentation symb 'structure) :structure)
  ((documentation symb 'function) :function)))

(defun validate-package (pkg)
 "VALIDATE-PACKAGE PKG => FAILURES

  FAILURES: FAILURE*
  FAILURE: (:failure SYMB MSG)

ARGUMENTS AND VALUES:

  PKG: A package symbol
  SYMB: Symbol the check failed on
  MSG: Message containing information about the failure

DESCRIPTION:

  VALIDATE-PACKAGE takes in PKG and validates that all the external symbols
  adhere to documentation guidelines, exist, and can be parsed to be used
  for exporting.

  Only one error per symbol will be reported at a time, all concatenated to
  a list in the aforementioned form."
 (macrolet
  ((with-success-check (symb &rest f)
    `(handler-case
      (progn ,@f :success)
      (validation-failure (v) (list :failure ,symb (validation-failure-msg v))))))
  (let
   ((symbs nil))
   (do-external-symbols (symb pkg) (push symb symbs))
   (setf symbs (sort symbs #'string< :key #'symbol-name))
   (remove :success
    (append
     (list (with-success-check pkg (docgen-pkg:doc->ast (find-package pkg))))
     (mapcar
      (lambda (symb)
       (with-success-check symb
        (case (get-symb-type symb)
         (:function (docgen-func:doc->ast symb))
         (:structure (docgen-struc:doc->ast symb))
         (t (error (make-condition 'validation-failure :msg (format nil "Symbol ~A has no documentation" symb)))))))
      symbs))))))

(defun pretty-print-validate-packages (&rest pkgs)
 "PRETTY-PRINT-VALIDATE-PACKAGES &rest PKGS => SUCCESS

  PKGS: PKG*

ARGUMENTS AND VALUES:

  SUCCESS: Whether or not all symbols passed validation
  PKG: A package symbol

DESCRIPTION:

  PRETTY-PRINT-VALIDATE-PACKAGES takes PKGS and runs validation on all of them.
  It dumps to standard out failures as it comes upon them, finally returning
  whether it was successful or not.

  This can be used in travis tests to ensure that documentation can be generated
  at a later date.

EXAMPLES:

  (pretty-print-validate-packages :pkg1 :pkg2) => t"
 (every
  #'identity
  (mapcar
   (lambda (pkg)
    (let
     ((failures (validate-package pkg)))
     (mapcar
      (lambda (failure)
       (format t "In ~A : ~A, documentation error found:~%  ~A~%" pkg (second failure) (third failure)))
      failures)
     (not failures)))
   pkgs)))

(defun table-of-contents (pkg)
 (format nil "## Contents~%~%~{~{* **~A [~A](#~A)** - ~A~}~%~}"
  (let
   ((symbs nil))
   (do-external-symbols (symb pkg) (push symb symbs))
   (setf symbs (sort symbs #'string< :key #'symbol-name))
   (mapcar
    (lambda (symb)
     (case (get-symb-type symb)
      (:function
       (list
        (docgen-func:ast->category-name (docgen-func:doc->ast symb))
        (docgen-func:ast->short-name (docgen-func:doc->ast symb))
        (docgen-func:ast->link (docgen-func:doc->ast symb))
        (docgen-func:ast->short-desc (docgen-func:doc->ast symb))))
      (:structure
       (list
        (docgen-struc:ast->category-name (docgen-struc:doc->ast symb))
        (docgen-struc:ast->short-name (docgen-struc:doc->ast symb))
        (docgen-struc:ast->link (docgen-struc:doc->ast symb))
        (docgen-struc:ast->short-desc (docgen-struc:doc->ast symb))))))
    symbs))))

(defun export-package (pkg)
 "EXPORT-PACKAGE PKG => MARKDOWN

ARGUMENTS AND VALUES:

  PKG: A package symbol
  MARKDOWN: A string containing the markdown representation of this packages documentation

DESCRIPTION:

  EXPORT-PACKAGE takes in PKG and converts all the documentation for the symbols
  into markdown with the hope of emulating the hyperspec style.

  It should only be run after the package has been validated, as it assumes that
  all documentation it gets will be valid."
 (let
  ((symbs nil))
  (do-external-symbols (symb pkg) (push symb symbs))
  (setf symbs (sort symbs #'string< :key #'symbol-name))
  (with-output-to-string (str)
   (format str "~A~%~%" (docgen-pkg:ast->md (docgen-pkg:doc->ast (find-package pkg))))
   (format str "~A~%" (table-of-contents pkg))
   (format str "~{~A~^~%~}"
    (mapcar
     (lambda (symb)
      (case (get-symb-type symb)
       (:function (docgen-func:ast->md (docgen-func:doc->ast symb)))
       (:structure (docgen-struc:ast->md (docgen-struc:doc->ast symb)))))
     symbs)))))
