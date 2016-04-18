(asdf:defsystem docgen
 :name "Documentation Generator"
 :version "0.1"
 :maintainer "Frank Duncan (frank@kank.com)"
 :author "Frank Duncan (frank@kank.com)"
 :serial t
 :components ((:file "package") (:file "func") (:file "pkg") (:file "struc") (:file "docgen"))
 :depends-on (#-travis :cl-ppcre)) ; Don't load libraries in travis
