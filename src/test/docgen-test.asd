; For why this is the way it is, see src/main/style-checker.asd
(asdf:defsystem docgen-test.internal
  :components ((:file "package")
               (:file "main")
               (:file "failures")))

(asdf:defsystem docgen-test
  :name "Document Generator Tests"
  :version "0.0.1"
  :maintainer "Frank Duncan (frank@kank.com)"
  :author "Frank Duncan (frank@kank.com)"
  :serial t
  :depends-on (:docgen docgen-test.internal))
