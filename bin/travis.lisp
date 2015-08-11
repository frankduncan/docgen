(setf *compile-print* nil)
(require 'asdf)
(setf asdf:*central-registry* (list #p"deps/"))
(asdf:load-system :docgen.internal)
(asdf:load-system :docgen-test.internal)

(format t "~%~c[1;33mRunning Tests~c[0m~%" #\Esc #\Esc)
(when (not (docgen-test:run-all-tests))
 (format t "~c[1;31mFailed tests!~c[0m~%" #\Esc #\Esc)
 (sb-ext:exit :code 1))

(format t "~%~c[1;33mChecking Style~c[0m~%" #\Esc #\Esc)
(when (not (syntax-checker:pretty-print-check-directory "src"))
 (format t "~c[1;31mFailed style check!~c[0m~%" #\Esc #\Esc)
 (sb-ext:exit :code 1))

(format t "~c[1;32mSuccess!~c[0m~%" #\Esc #\Esc)
(sb-ext:exit :code 0)