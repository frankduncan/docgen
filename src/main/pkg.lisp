(in-package #:docgen-pkg)

(defun fire-error (msg) (error (make-instance 'docgen:validation-failure :msg msg)))

(defun doc->ast (pkg)
 (when (not (documentation pkg t)) (fire-error (format nil "Package ~A has no documentation" (package-name pkg))))
 (labels
  ((validate (strs)
    (mapcar
     (lambda (str)
      (cond
       ((< 120 (length str)) (fire-error (format nil "Package description longer than 120 characters: ~A" str)))
       ((cl-ppcre:scan "^ " str) (fire-error (format nil "Package description line started with space: ~A" str)))
       ((cl-ppcre:scan " $" str) (fire-error (format nil "Package description line ended with space: ~A" str)))))
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
   ((lines (cl-ppcre:split "\\n" (documentation pkg t))))
   (validate lines)
   (let
    ((paragraphs (combine lines)))
    (when (< 120 (length (first paragraphs))) (fire-error "First package paragraph is longer than 120 characters"))
    (when (find "" paragraphs :test #'string=) (fire-error "Package description has two empty lines in it"))
    (cons (package-name pkg) paragraphs)))))

(defun ast->md (ast)
 (format nil "# Package ~A~%~%~{~A~^~%~%~}"
  (car ast)
  (cdr ast)))
