#!/bin/bash

sbcl \
  --eval "(asdf:load-system :docgen)" \
  --eval "(load \"$1\")" \
  --eval "(format t \"----~%\")" \
  --eval "(format t \"~A\" (docgen:export-package $2))" \
  --eval "(quit)" 2> /dev/null | sed -n '/^----$/,$p' | tail -n +2 > fromcl.md

vimdiff fromcl.md ${1/lisp/md}
rm fromcl.md
