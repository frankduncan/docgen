#!/bin/bash

sbcl \
  --eval "(asdf:load-system :docgen)" \
  --eval "(format t \"----~%\")" \
  --eval "(format t \"~A\" (docgen:export-package :docgen))" \
  --eval "(quit)" 2> /dev/null | sed -n '/^----$/,$p' | tail -n +2 > wiki/Home.md
