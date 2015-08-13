(in-package #:docgen)

(define-condition validation-failure nil ((msg :initarg :msg :reader validation-failure-msg)))

(defun get-symb-type (symb)
 (cond
  ;((documentation symb 'variable) :variable)
  ;((documentation symb 'structure) :structure)
  ((documentation symb 'function) :function)))

(defun validate-package (pkg)
 (macrolet
  ((with-success-check (&rest f)
    `(handler-case
      (progn ,@f :success)
      (validation-failure (v) (list :failure (validation-failure-msg v))))))
  (let
   ((symbs nil))
   (do-external-symbols (symb pkg) (push symb symbs))
   (setf symbs (sort symbs #'string< :key #'symbol-name))
   (remove :success
    (append
     (list (with-success-check (docgen-pkg:doc->ast (find-package pkg))))
     (mapcar
      (lambda (symb)
       (with-success-check
        (case (get-symb-type symb)
         (:function (docgen-func:doc->ast symb))
         (t (error (make-condition 'validation-failure :msg (format nil "Symbol ~A has no documentation" symb)))))))
      symbs))))))

(defun pretty-print-validate-packages (&rest pkgs)
 (mapcar
  (lambda (pkg)
   (let
    ((failures (validate-package pkg)))
    (mapcar
     (lambda (failure)
      (format t "In package ~A, documentation error found:~%  ~A" pkg (cadr failure)))
     failures)
    (not failures)))
  pkgs))

(defun export-package (pkg)
 (let
  ((symbs nil))
  (do-external-symbols (symb pkg) (push symb symbs))
  (setf symbs (sort symbs #'string< :key #'symbol-name))
  (with-output-to-string (str)
   (format str "~A~%~%" (docgen-pkg:ast->md (docgen-pkg:doc->ast (find-package pkg))))
   (format str "~{~A~^~%~}"
    (mapcar
     (lambda (symb)
      (case (get-symb-type symb)
       (:function (docgen-func:ast->md (docgen-func:doc->ast symb)))))
     symbs)))))
