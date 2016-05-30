(asdf:defsystem docgen
 :name "Documentation Generator"
 :version "0.3"
 :maintainer "Frank Duncan (frank@kank.com)"
 :author "Frank Duncan (frank@kank.com)"
 :serial t
 :components ((:file "package") (:file "func") (:file "var") (:file "pkg") (:file "struc") (:file "docgen"))
 :depends-on (#-travis :cl-ppcre)) ; Don't load libraries in travis
