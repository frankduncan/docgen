(asdf:defsystem docgen-test
 :name "Document Generator Tests"
 :version "0.1"
 :maintainer "Frank Duncan (frank@kank.com)"
 :author "Frank Duncan (frank@kank.com)"
 :serial t
 :components ((:file "package") (:file "main") (:file "failures"))
 :depends-on (:docgen))
