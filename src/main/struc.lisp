(in-package #:docgen-struc)

(defun fire-error (msg) (error (make-instance 'docgen:validation-failure :msg msg)))

(defun doc->ast (struc)
 (labels
  ((validate (strs)
    (mapcar
     (lambda (str)
      (cond
       ((< 120 (length str)) (fire-error (format nil "Structure description longer than 120 characters: ~A" str)))
       ((cl-ppcre:scan "^ " str) (fire-error (format nil "Structure description line started with space: ~A" str)))
       ((cl-ppcre:scan " $" str) (fire-error (format nil "Structure description line ended with space: ~A" str)))))
     strs))
   (combine (strs)
    (cond
     ((not strs) (list ""))
     ((string= "" (car strs)) (cons "" (combine (cdr strs))))
     (t
      (let
       ((rest (combine (cdr strs))))
       (cons (format nil "~A~A~A" (car strs) (if (string/= "" (car rest)) " " "") (car rest)) (cdr rest)))))))
  (let
   ((lines (cl-ppcre:split "\\n" (documentation struc 'structure))))
   (validate lines)
   (let
    ((paragraphs (combine lines)))
    (when (find "" paragraphs :test #'string=) (fire-error "Structure description has two empty lines in it"))
    (cons
     (cond
      ((typep (make-instance struc) 'condition) :condition)
      (t :struct))
     (cons struc paragraphs))))))

(defun ast->md (ast)
 (format nil "## ~@(~A~) ~A~%~%~{~A~%~^~%~}"
  (first ast)
  (second ast)
  (cddr ast)))

(defun ast->category-name (ast)
 (case (first ast)
  (:condition "condition")
  (t "structure")))

(defun ast->short-name (ast)
 (format nil "~(~A~)" (second ast)))

(defun ast->link (ast)
 (format nil "~(~A-~A~)" (first ast) (second ast)))

(defun ast->short-desc (ast)
 (third ast))
