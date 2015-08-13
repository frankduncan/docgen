(in-package #:docgen)

(define-condition validation-failure nil ((msg :initarg :msg :reader validation-failure-msg)))

(defun validate-package (pkg)
 (macrolet
  ((with-success-check (&rest f)
    `(handler-case
      (progn ,@f :success)
      (validation-failure (v) (list :failure :msg (validation-failure-msg v))))))
  (let
   ((symbs nil))
   (do-external-symbols (symb pkg) (push symb symbs))
   (setf symbs (sort symbs #'string< :key #'symbol-name))
   (remove :success
    (append
     (list (with-success-check (docgen-pkg:doc->ast pkg)))
     (mapcar
      (lambda (symb) (with-success-check (docgen-func:doc->ast symb)))
      symbs))))))

(defun export-package (pkg)
 (let
  ((symbs nil))
  (do-external-symbols (symb pkg) (push symb symbs))
  (setf symbs (sort symbs #'string< :key #'symbol-name))
  (with-output-to-string (str)
   (format str "~A~%~%" (docgen-pkg:ast->md (docgen-pkg:doc->ast (find-package pkg))))
   (format str "~{~A~^~%~}" (mapcar (lambda (symb) (docgen-func:ast->md (docgen-func:doc->ast symb))) symbs)))))
