(in-package #:docgen)

(define-condition validation-failure nil ((msg :initarg :msg :reader validation-failure-msg)))

(defun validate-package (pkg)
 (let
  ((symbs nil))
  (do-external-symbols (symb pkg) (push symb symbs))
  (setf symbs (sort symbs #'string< :key #'symbol-name))
  (remove :success
   (mapcar
    (lambda (symb)
     (handler-case
      (progn
       (docgen-func:doc->ast symb)
       :success)
      (validation-failure (v) (list :failure :msg (validation-failure-msg v)))))
    symbs))))

(defun export-package (pkg)
 (let
  ((symbs nil))
  (do-external-symbols (symb pkg) (push symb symbs))
  (setf symbs (sort symbs #'string< :key #'symbol-name))
  (format nil "~{~A~^~%~}" (mapcar (lambda (symb) (docgen-func:ast->md (docgen-func:doc->ast symb))) symbs))))
