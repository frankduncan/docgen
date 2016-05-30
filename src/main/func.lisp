(in-package #:docgen-func)

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

(defun decompose-type (type-line)
 (labels
  ((decompose-symbol (atom)
    (cond
     ((cl-ppcre:scan " " atom) (fire-error (format nil "Symbols had spaces in it: ~A" atom)))
     ((cl-ppcre:scan ":.*[A-Z]" atom) (fire-error (format nil "Keyword symbols must all be lower case: ~A" atom)))
     ((cl-ppcre:scan ":.*" atom) (list :keyword atom))
     ((cl-ppcre:scan "[a-z]" atom) (fire-error (format nil "Type symbols must all be upper case: ~A" atom)))
     (t (list :type atom))))
   (asterisk-type (atom)
    (when (cl-ppcre:scan "\\*$" atom)
     (list :asterisk (list (decompose-symbol (subseq atom 0 (1- (length atom))))))))
   (symbol-type (def)
    (list :symbol (list (decompose-symbol def))))
   (or-type (def)
    (when (cl-ppcre:scan "\\|" def)
     (when (cl-ppcre:scan "[\\(\\)]" def) (fire-error (format nil "Or types can't have lists in them: ~A" def)))
     (when (cl-ppcre:scan "[^ ]\\|" def)
      (fire-error (format nil "All or pipes must be prefaced by spaces: ~A" def)))
     (when (cl-ppcre:scan "\\|[^ ]" def)
      (fire-error (format nil "All or pipes must be concluded by spaces: ~A" def)))
     (list :or (mapcar #'decompose-symbol (cl-ppcre:split " \\| " def)))))
   (list-type (def)
    (when
     (cl-ppcre:scan "^\\(.*\\)$" def)
     (when (cl-ppcre:scan "\\|" def) (fire-error (format nil "List types can't have | in them: ~A" def)))
     (when (cl-ppcre:scan "^\\(.*[\\(\\)].*\\)$" def)
      (fire-error (format nil "List types can't have sublists: ~A" def)))
     (when (cl-ppcre:scan "  " def) (fire-error (format nil "Lists can be seperated by only one space: ~A" def)))
     (list
      :list
      (mapcar #'decompose-symbol (cl-ppcre:split " " (subseq def 1 (1- (length def)))))))))
  (let
   ((type-scanner (cl-ppcre:create-scanner "^  ([^ :]+): (.+)$")))
   (when (not (cl-ppcre:scan type-scanner type-line))
    (fire-error (format nil "Type line did not match \"  TYPE: type-definition\": ~A" type-line)))
   (cl-ppcre:register-groups-bind (type def) (type-scanner type-line)
    (let
     ((decomposed-def (or (or-type def) (list-type def) (asterisk-type def) (symbol-type def))))
     (when (not decomposed-def) (fire-error (format nil "Couldn't figure out how to decompose: ~A" def)))
     (list
      type
      decomposed-def
      (remove
       nil
       (mapcar
        (lambda (symb-def) (when (eql :type (car symb-def)) (cadr symb-def)))
        (cadr decomposed-def)))))))))

(defun parse-types (types)
 (if
  (string/= "ARGUMENTS AND VALUES:" (peek))
  (multiple-value-bind (processed-types args-to-be-explained) (process-types types)
   (expect-blank-line)
   (values (list :types processed-types) args-to-be-explained))
  (values nil types)))

(defun process-types (remaining-types &optional processed-types args-to-be-explained)
 (verify-next-line)
 (cond
  ((string= "" (peek)) (values processed-types (append args-to-be-explained remaining-types)))
  ((not remaining-types)
   (fire-error (format nil "Ran out of types to talk about, but got a non empty line: ~A" (peek))))
  (t
   (let
    ((decomposed (decompose-type (peek))))
    (if (string/= (car decomposed) (car remaining-types))
     (process-types (cdr remaining-types) processed-types (append args-to-be-explained (list (car remaining-types))))
     (progn
      (next)
      (process-types
       (append (cdr remaining-types) (third decomposed))
       (append processed-types (list (list (car decomposed) (second decomposed))))
       args-to-be-explained)))))))

(defun description->paragraphs ()
 (verify-next-line :optional t)
 (let
  ((next-line (next)))
  (cond
   ((not next-line) (list "")) ; Can be last section
   ((and (string= "" next-line) (not (more))) (fire-error "Can't end with empty line"))
   ((cl-ppcre:scan "^  [^ ].+" next-line)
    (let
     ((rest-of-description (description->paragraphs)))
     (cons
      (format nil "~A~A~A"
       (subseq next-line 2 (length next-line))
       (if (and (car rest-of-description) (string/= "" (car rest-of-description))) " " "")
       (car rest-of-description))
      (cdr rest-of-description))))
   ((string= "" next-line)
    (if (string= "EXAMPLES:" (peek))
     (list "")
     (cons "" (description->paragraphs))))
   (t (fire-error (format nil "Got unexpected line, requires blank lines or start with two spaces: ~S" next-line))))))

(defun parse-description ()
 (when (string/= "DESCRIPTION:" (next)) (fire-error (format nil "Expected DESCRIPTION: instead of: ~A" (prev-line))))
 (expect-blank-line)
 (let
  ((paragraphs (description->paragraphs)))
  (list :description (mapcar #'handle-text paragraphs))))

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

(defun process-argument-and-values (args-to-be-explained)
 (verify-next-line)
 (cond
  ((string= "" (peek))
   (when args-to-be-explained
    (fire-error (format nil "Unexplained arguments left: ~A" args-to-be-explained))))
  ((not args-to-be-explained) (fire-error (format nil "No arguments left, but next line isn't empty: ~A" (peek))))
  (t
   (labels
    ((decompose-arg (arg-line)
      (let
       ((arg-scanner (cl-ppcre:create-scanner "^  ([^ :]+): (.+)$")))
       (when (not (cl-ppcre:scan arg-scanner arg-line))
        (fire-error (format nil "Argument line did not match \"  TYPE: desc\": ~A" arg-line)))
       (cl-ppcre:register-groups-bind (arg desc) (arg-scanner arg-line)
        (list arg desc)))))
    (let
     ((decomposed (decompose-arg (next))))
     (when
      (string/= (car args-to-be-explained) (car decomposed))
      (fire-error (format nil "Expected a description for ~A but got one for ~A"
                   (car args-to-be-explained)
                   (car decomposed))))
     (cons
      (list
       (car decomposed)
       (handle-text (cadr decomposed)))
      (process-argument-and-values (cdr args-to-be-explained))))))))

(defun parse-arguments-and-values (args-to-be-explained)
 (when (string/= "ARGUMENTS AND VALUES:" (next))
  (fire-error (format nil "Expected ARGUMENTS AND VALUES: instead of: ~A" (prev-line))))
 (expect-blank-line)
 (let
  ((processed-args-and-values (process-argument-and-values (remove-duplicates args-to-be-explained :test #'string=))))
  (expect-blank-line)
  (list :arguments-and-values processed-args-and-values)))

(defun parse-header (func)
 (verify-next-line)
 (let*
  ((func-name (symbol-name func))
   (scanner (cl-ppcre:create-scanner (format nil "~A(.*) => (.*)$" func-name))))
  (when (not (cl-ppcre:scan scanner (peek)))
   (fire-error (format nil "First line of ~A did not match: ~A {ARGS}* => {RESULT}*, ~A" func func-name (peek))))
  (cl-ppcre:register-groups-bind (args result) (scanner (next))
   (when (cl-ppcre:scan "[a-z]" func-name)
    (fire-error (format nil "Function name should be all uppercase: ~A" func-name)))
   (let
    ((ast-of-start
      (list
       func-name
       (mapcar
        (lambda (arg)
         (cond
          ((cdr (assoc arg '(("&optional" . :&optional) ("&key" . &key) ("&rest" . &rest)) :test #'string=)))
          ((cl-ppcre:scan "[a-z]" arg)
           (fire-error (format nil "Argument in ~A should be all upper case: ~S" func-name arg)))
          (t arg)))
        (cdr (cl-ppcre:split " " args)))
       (mapcar
        (lambda (arg)
         (cond
          ((cl-ppcre:scan "[a-z]" arg)
           (fire-error (format nil "Result in ~A should be all upper case: ~A" func-name arg)))
          (t arg)))
        (cl-ppcre:split ", " result)))))
    (add-keyword func-name)
    (expect-blank-line)
    (values
     (cons :function ast-of-start)
     (remove-if-not #'stringp (append (second ast-of-start) (third ast-of-start))))))))

(defun internal-doc->ast (func doc)
 (let
  ((*doc* (cl-ppcre:split "\\n" doc :limit 1000))
   (*prev-line* nil)
   (*keywords* nil))
  (multiple-value-bind (header types) (parse-header func)
   (mapcar #'add-keyword types)
   (cons header
    (multiple-value-bind (types args-to-be-defined) (parse-types types)
     (mapcar #'add-keyword args-to-be-defined)
     (append
      (when types (list types))
      (list
       (when args-to-be-defined (parse-arguments-and-values args-to-be-defined))
       (parse-description))
      (when (more) (list (parse-examples)))))))))

(defun doc->ast (func) (internal-doc->ast func (documentation func 'function)))

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
 (format nil "## Function **~A**

#### Syntax:

**~(~A~)** ~{_~(~A~)_ ~}=> ~{_~(~A~)_~^, ~}

"
  (second header)
  (second header)
  (third header)
  (fourth header)))

(defun format-types (types)
 (flet
  ((recompose-type (type)
    (case (car type)
     (:list (format nil "(~{~(~A~)~^ ~})" (mapcar #'cadr (cadr type))))
     (:or (format nil "~{~(~A~)~^ | ~}" (mapcar #'cadr (cadr type))))
     (:asterisk (format nil "~(~A~)*" (cadr (car (cadr type)))))
     (:symbol (format nil "~(~A~)" (cadr (car (cadr type))))))))
  (if (not types)
   ""
   (format nil "~{~A~%~}~%"
    (mapcar
     (lambda (type) (format nil "```~(~A~)::= ~A```  " (car type) (recompose-type (cadr type))))
     (cadr types))))))

(defun format-args-and-values (args-and-values)
 (format nil "#### Arguments and Values:~%~%~{~A~%~}~%"
  (mapcar
   (lambda (arg-value) (format nil "_~(~A~)_---~A  " (car arg-value) (format-text (cadr arg-value))))
   (cadr args-and-values))))

(defun format-description (description)
 (format nil "#### Description:~%~%~{~A~%~^~%~}" (mapcar #'format-text (cadr description))))

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
  (format nil "~A~A~A~A~A"
   (format-header (get-section :function))
   (format-types (get-section :types))
   (if
    (get-section :arguments-and-values)
    (format-args-and-values (get-section :arguments-and-values))
    "")
   (format-description (get-section :description))
   (format-examples (get-section :examples)))))

(defun ast->category-name (ast)
 (declare (ignore ast))
 "function")

(defun ast->short-name (ast)
 (format nil "~(~A~)" (second (find :function ast :key #'car))))

(defun ast->link (ast)
 (format nil "function-~(~A~)" (second (find :function ast :key #'car))))

(defun ast->short-desc (ast)
 (format-text (car (cadr (find :description ast :key #'car)))))
