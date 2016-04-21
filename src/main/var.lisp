(in-package #:docgen-var)

(defvar *doc*)
(defvar *prev-line*)
(defun peek () (car *doc*))
(defun next () (setf *prev-line* (pop *doc*)))
(defun more () (not (not *doc*)))
(defun prev-line () *prev-line*)

(defvar *keywords*)

(defun add-keyword (type)
 (setf *keywords* (remove-duplicates (cons type *keywords*) :test #'string=)))

(defun fire-error (msg) (error (make-instance 'docgen:validation-failure :msg msg)))

(defun expect-blank-line ()
 (let
  ((prev (prev-line)))
  (when (string/= "" (next)) (fire-error (format nil "Expected blank line after: ~A" prev)))))

(defun verify-next-line (&key optional)
 (cond
  ((and optional (not (more))) t)
  ((not (more)) (fire-error (format nil "Expected line after: ~A" (prev-line))))
  ((cl-ppcre:scan " $" (peek)) (fire-error (format nil "Can't end line with a space: ~A" (peek))))
  ((< 120 (length (peek))) (fire-error (format nil "Longer than 120 chars: ~A" (peek))))))

(defun freeform->paragraphs (next next-optional)
 (verify-next-line :optional t)
 (let
  ((next-line (next)))
  (cond
   ((and next-optional (not next-line)) (list ""))
   ((and (string= "" next-line) (not (more))) (fire-error "Can't end with empty line"))
   ((cl-ppcre:scan "^  [^ ].+" next-line)
    (let
     ((rest-of-freeform (freeform->paragraphs next next-optional)))
     (cons
      (format nil "~A~A~A"
       (subseq next-line 2 (length next-line))
       (if (and (car rest-of-freeform) (string/= "" (car rest-of-freeform))) " " "")
       (car rest-of-freeform))
      (cdr rest-of-freeform))))
   ((string= "" next-line)
    (if (string= next (peek))
     (list "")
     (cons "" (freeform->paragraphs next next-optional))))
   (t (fire-error (format nil "Got unexpected line, requires blank lines or start with two spaces: ~S" next-line))))))

(defun parse-freeform (start section next next-optional)
 (when (string/= start (next)) (fire-error (format nil "Expected ~A instead of: ~A" start (prev-line))))
 (expect-blank-line)
 (let
  ((paragraphs (freeform->paragraphs next next-optional)))
  (list section (mapcar #'handle-text paragraphs))))

(defun process-examples ()
 (when (more)
  (verify-next-line :optional t)
  (cons
   (let
    ((example-scanner (cl-ppcre:create-scanner "^  ([^ ].+) => (.+)$"))
     (next-line (next)))
    (if (not (cl-ppcre:scan example-scanner next-line))
     (fire-error (format nil "Example line does not match \"  example => result\": ~A" next-line))
     (cl-ppcre:register-groups-bind (example result) (example-scanner next-line)
      (list example result))))
   (process-examples))))

(defun parse-examples ()
 (when (string/= "EXAMPLES:" (next)) (fire-error (format nil "Expected EXAMPLES: instead of: ~A" (prev-line))))
 (expect-blank-line)
 (list :examples (process-examples)))

; For formatting of things like types in there
(defun handle-text (text)
 (labels
  ((inject-keywords (text remaining-keywords)
    (if
     (not remaining-keywords)
     (list text)
     (apply #'append
      (mapcar
       (lambda
        (text-item)
        (cond
         ((not (stringp text-item)) (list text-item))
         ((not (cl-ppcre:scan (cl-ppcre:quote-meta-chars (car remaining-keywords)) text-item)) (list text-item))
         (t
          (let
           ((split-text (cl-ppcre:split (cl-ppcre:quote-meta-chars (car remaining-keywords)) text-item :limit 1000)))
           (apply #'append
            (list (car split-text))
            (mapcar (lambda (ti) (list (list :keyword (car remaining-keywords)) ti)) (cdr split-text)))))))
       (inject-keywords text (cdr remaining-keywords)))))))
  (list :text (inject-keywords text *keywords*))))
; (map
; (list :text text))

(defun parse-header (var)
 (verify-next-line)
 (let*
  ((var-name (symbol-name var)))
  (when (not (string= var-name (peek)))
   (fire-error (format nil "First line of ~A did not match: ~A, ~A" var var-name (peek))))
  (when (cl-ppcre:scan "[a-z]" var-name)
   (fire-error (format nil "Variable name should be all uppercase: ~A" var-name)))
  (add-keyword var-name)
  (next)
  (expect-blank-line)
  (list :variable var-name)))

(defun internal-doc->ast (var doc)
 (let
  ((*doc* (cl-ppcre:split "\\n" doc :limit 1000))
   (*prev-line* nil)
   (*keywords* nil))
  (cons (parse-header var)
   (append
    (list
     (parse-freeform "VALUE TYPE:" :value-type "INITIAL VALUE:" nil)
     (parse-freeform "INITIAL VALUE:" :initial-value "DESCRIPTION:" nil)
     (parse-freeform "DESCRIPTION:" :description "EXAMPLES:" t))
    (when (more) (list (parse-examples)))))))

(defun doc->ast (var) (internal-doc->ast var (documentation var 'variable)))

(defun format-text (text)
 (format nil "~{~A~}"
  (mapcar
   (lambda (text)
    (cond
     ((stringp text) text)
     ((and (listp text) (eql :keyword (car text))) (format nil "_~(~A~)_" (cadr text)))
     (t (fire-error (format nil "Don't know how to convert text: ~S" text)))))
   (cadr text))))

(defun format-header (header)
 (format nil "## Variable ~A

"
  (cl-ppcre:quote-meta-chars (second header))))

(defun format-freeform (heading text)
 (format nil "#### ~A:~%~%~{~A~%~^~%~}" heading (mapcar #'format-text (cadr text))))

(defun format-examples (examples)
 (if (not examples)
  ""
  (format nil "~%#### Examples:~%~%~{~A~%~}"
   (mapcar
    (lambda (example) (format nil "```~A``` => ```~A```  " (car example) (cadr example)))
    (cadr examples)))))

(defun ast->md (ast)
 (flet
  ((get-section (name) (find name ast :key #'car)))
  (format nil "~A~A~%~A~%~A~A"
   (format-header (get-section :variable))
   (format-freeform "Value Type" (get-section :value-type))
   (format-freeform "Initial Value" (get-section :initial-value))
   (format-freeform "Description" (get-section :description))
   (format-examples (get-section :examples)))))

(defun ast->category-name (ast)
 (declare (ignore ast))
 "variable")

(defun ast->short-name (ast)
 (format nil "~(~A~)" (cl-ppcre:quote-meta-chars (second (find :variable ast :key #'car)))))

(defun ast->link (ast)
 (format nil "variable-~(~A~)" (cl-ppcre:regex-replace-all "\\*" (second (find :variable ast :key #'car)) "")))

(defun ast->short-desc (ast)
 (format-text (car (cadr (find :description ast :key #'car)))))
