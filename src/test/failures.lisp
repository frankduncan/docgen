(in-package #:docgen-test)

(let
 ((long-line (format nil "~A~A"
              "This second section uses PATH and X as something we should talk about, "
              "but doesn't use all the arguments (let's include PATH here for fun)")))
 (deffailure-func-test
  "Long line"
  (format nil "UNUSED => RESULT

ARGUMENTS AND VALUES:

  RESULT: a pathname

DESCRIPTION:

  ~A"
   long-line)
  (format nil "Longer than 120 chars:   ~A" long-line)))

(deffailure-func-test
 "Blank line - after args and vals"
 "UNUSED => RESULT

ARGUMENTS AND VALUES:
  RESULT: a pathname

DESCRIPTION:

"
 "Expected blank line after: ARGUMENTS AND VALUES:")

(deffailure-func-test
 "Blank line - after description"
 "UNUSED => RESULT

ARGUMENTS AND VALUES:

  RESULT: a pathname

DESCRIPTION:
  Fail here

"
 "Expected blank line after: DESCRIPTION:")

(deffailure-func-test
 "Blank line - after examples"
 "UNUSED => RESULT

ARGUMENTS AND VALUES:

  RESULT: a pathname

DESCRIPTION:

  Fail here

EXAMPLES:
  Fail here

"
 "Expected blank line after: EXAMPLES:")

(deffailure-func-test
 "Blank line - after header"
 "UNUSED => RESULT
 Fail here
"
 "Expected blank line after: UNUSED => RESULT")

(deffailure-func-test
 "Two spaces - beginning of types"
 "UNUSED => RESULT

  RESULT: RESULT1
   RESULT1: NOT-HERE
"
 "Type line did not match \"  TYPE: type-definition\":    RESULT1: NOT-HERE")

(deffailure-func-test
 "Two spaces - beginning of args and values"
 "UNUSED => RESULT

ARGUMENTS AND VALUES:

   RESULT: fail here
"
 "Argument line did not match \"  TYPE: desc\":    RESULT: fail here")

(deffailure-func-test
 "Two spaces - in description"
 "UNUSED => RESULT

ARGUMENTS AND VALUES:

  RESULT: a result

DESCRIPTION:

  This is a description

  About some
   things"
 "Got unexpected line, requires blank lines or start with two spaces: \"   things\"")

(deffailure-func-test
 "Two spaces - in examples"
 "UNUSED => RESULT

ARGUMENTS AND VALUES:

  RESULT: a result

DESCRIPTION:

  This is a description

EXAMPLES:

  (example1) => (yo)
   (example2) => (yoyo)"
 "Example line does not match \"  example => result\":    (example2) => (yoyo)")

(deffailure-func-test
 "Two spaces - in examples"
 "UNUSED => RESULT

ARGUMENTS AND VALUES:

  RESULT: a result

DESCRIPTION:

  This is a description

EXAMPLES:

  (example1) => (yo)
   (example2) => (yoyo)"
 "Example line does not match \"  example => result\":    (example2) => (yoyo)")

(deffailure-func-test
 "Bad type - lowercase symbol"
 "UNUSED => REsULT

  RESULT: RESULT1

"
 "Result in UNUSED should be all upper case: REsULT")

(deffailure-func-test
 "Bad type - or type with list"
 "UNUSED => RESULT

  RESULT: RESULT1 | (RESULT2 :success)

"
 "Or types can't have lists in them: RESULT1 | (RESULT2 :success)")

(deffailure-func-test
 "Bad type - or type with no space before pipe"
 "UNUSED => RESULT

  RESULT: RESULT1| RESULT2

"
 "All or pipes must be prefaced by spaces: RESULT1| RESULT2")

(deffailure-func-test
 "Bad type - or type with no space after pipe"
 "UNUSED => RESULT

  RESULT: RESULT1 |RESULT2

"
 "All or pipes must be concluded by spaces: RESULT1 |RESULT2")

(deffailure-func-test
 "Bad type - list separated by multiple spaces"
 "UNUSED => RESULT

  RESULT: (RESULT1  :success)

"
 "Lists can be seperated by only one space: (RESULT1  :success)")

(deffailure-func-test
 "Bad type - list parens in them"
 "UNUSED => RESULT

  RESULT: (RESULT1 (:success))

"
 "List types can't have sublists: (RESULT1 (:success))")

(deffailure-func-test
 "Bad type - type line doesn't have colon"
 "UNUSED => RESULT

  RESULT - RESULT1

"
 "Type line did not match \"  TYPE: type-definition\":   RESULT - RESULT1")

(deffailure-func-test
 "Bad type - malformed type line with colon"
 "UNUSED => RESULT

  RESULT: RESULT1 RESULT2

"
 "Symbols had spaces in it: RESULT1 RESULT2")

(deffailure-func-test
 "types in type section that isn't in document"
 "UNUSED => RESULT

  RESULT: RESULT1
  RESULT2: RESULT3

"
 "Ran out of types to talk about, but got a non empty line:   RESULT2: RESULT3")

(deffailure-func-test
 "Description - ends with empty line when last thing"
 "UNUSED => RESULT

ARGUMENTS AND VALUES:

  RESULT: a pathname

DESCRIPTION:

  Hello world

"
 "Can't end with empty line")

(deffailure-func-test
 "Description - malformed line"
 "UNUSED => RESULT

ARGUMENTS AND VALUES:

  RESULT: a pathname

DESCRIPTION:

 A mistake"
 "Got unexpected line, requires blank lines or start with two spaces: \" A mistake\"")

(deffailure-func-test
 "Description - section doesn't start with description"
 "UNUSED => RESULT

ARGUMENTS AND VALUES:

  RESULT: a pathname

DESCRIPTAION:

"
 "Expected DESCRIPTION: instead of: DESCRIPTAION:")

(deffailure-func-test
 "Examples - doesn't have arrow"
 "UNUSED => RESULT

ARGUMENTS AND VALUES:

  RESULT: a pathname

DESCRIPTION:

  This is a mock description.

EXAMPLES:

  (unused) - :success
"
 "Example line does not match \"  example => result\":   (unused) - :success")

(deffailure-func-test
 "Examples - doesn't start with EXAMPLES"
 "UNUSED => RESULT

ARGUMENTS AND VALUES:

  RESULT: a pathname

DESCRIPTION:

  This is a mock description.

EXAAMPLES:

  (unused) => :success"
 "Got unexpected line, requires blank lines or start with two spaces: \"EXAAMPLES:\"")

(deffailure-func-test
 "Args-and-values - leftover unexplained args"
 "UNUSED => RESULT

  RESULT: RESULT1 | RESULT2

ARGUMENTS AND VALUES:

  RESULT1: a pathname
"
 "Unexplained arguments left: (RESULT2)")

(deffailure-func-test
 "Args-and-values - doesn't match TYPE: desc"
 "UNUSED => RESULT

ARGUMENTS AND VALUES:

  RESULT - a pathname

DESCRIPTION:

"
 "Argument line did not match \"  TYPE: desc\":   RESULT - a pathname")

(deffailure-func-test
 "Args-and-values - section doesn't start with ARGUMENTS AND VALUES:"
 "UNUSED => RESULT

  RESULT: RESULT1

ARGUUMENTS AND VALUES:

  RESULT: a pathname

DESCRIPTION:

"
 "Expected ARGUMENTS AND VALUES: instead of: ARGUUMENTS AND VALUES:")

(deffailure-func-test
 "Header - first line doesn't start with func-name (naturally all in upper case)"
 "UNUUSED => RESULT

ARGUMENTS AND VALUES:

  RESULT: a pathname

DESCRIPTION:

"
 "First line of UNUSED did not match: UNUSED {ARGS}* => {RESULT}*, UNUUSED => RESULT")

(deffailure-func-test
 "Header - arguments weren't in upper cse"
 "UNUSED x => RESULT

ARGUMENTS AND VALUES:

  RESULT: a pathname

DESCRIPTION:

"
 "Argument in UNUSED should be all upper case: \"x\"")

(deffailure-func-test
 "Header - results weren't in upper case"
 "UNUSED => REsULT

ARGUMENTS AND VALUES:

  RESULT: a pathname

DESCRIPTION:

"
 "Result in UNUSED should be all upper case: REsULT")

(deffailure-var-test
 "Blank line - after value type"
 "*UNUSED*

VALUE TYPE:
  generalized boolean

INITIAL VALUE:

  NIL

DESCRIPTION:

"
 "Expected blank line after: VALUE TYPE:")

(deffailure-var-test
 "Blank line - after description"
 "*UNUSED*

VALUE TYPE:

  generalized boolean

INITIAL VALUE:

  NIL

DESCRIPTION:
  Fail here

"
 "Expected blank line after: DESCRIPTION:")

(deffailure-var-test
 "Blank line - after examples"
 "*UNUSED*

VALUE TYPE:

  generalized boolean

INITIAL VALUE:

  NIL

DESCRIPTION:

  Fail here

EXAMPLES:
  Fail here

"
 "Expected blank line after: EXAMPLES:")

(deffailure-var-test
 "Blank line - after header"
 "*UNUSED*
 Fail here
"
 "Expected blank line after: *UNUSED*")

(deffailure-var-test
 "Two spaces - beginning of value type"
 "*UNUSED*

VALUE TYPE:

   a generalized boolean
"
 "Got unexpected line, requires blank lines or start with two spaces: \"   a generalized boolean\"")

(deffailure-var-test
 "Two spaces - beginning of initial value"
 "*UNUSED*

VALUE TYPE:

  a generalized boolean

INITIAL VALUE:

   RESULT: fail here
"
 "Got unexpected line, requires blank lines or start with two spaces: \"   RESULT: fail here\"")

(deffailure-var-test
 "Two spaces - in description"
 "*UNUSED*

VALUE TYPE:

  a generalized boolean

INITIAL VALUE:

  RESULT: a result

DESCRIPTION:

  This is a description

  About some
   things"
 "Got unexpected line, requires blank lines or start with two spaces: \"   things\"")

(deffailure-var-test
 "Two spaces - in examples"
 "*UNUSED*

VALUE TYPE:

  a generalized boolean

INITIAL VALUE:

  RESULT: a result

DESCRIPTION:

  This is a description

EXAMPLES:

  (example1) => (yo)
   (example2) => (yoyo)"
 "Example line does not match \"  example => result\":    (example2) => (yoyo)")

(deffailure-var-test
 "Two spaces - in examples"
 "*UNUSED*

VALUE TYPE:

  a generalized boolean

INITIAL VALUE:

  RESULT: a result

DESCRIPTION:

  This is a description

EXAMPLES:

  (example1) => (yo)
   (example2) => (yoyo)"
 "Example line does not match \"  example => result\":    (example2) => (yoyo)")

(deffailure-var-test
 "Description - ends with empty line when last thing"
 "*UNUSED*

VALUE TYPE:

  a generalized boolean

INITIAL VALUE:

  NIL

DESCRIPTION:

  Hello world

"
 "Can't end with empty line")

(deffailure-var-test
 "Description - malformed line"
 "*UNUSED*

VALUE TYPE:

  a generalized boolean

INITIAL VALUE:

  NIL

DESCRIPTION:

 A mistake"
 "Got unexpected line, requires blank lines or start with two spaces: \" A mistake\"")

(deffailure-var-test
 "Description - section doesn't start with description"
 "*UNUSED*

VALUE TYPE:

  a generalized boolean

INITIAL VALUE:

  NIL

DESCRIPTAION:

"
 "Got unexpected line, requires blank lines or start with two spaces: \"DESCRIPTAION:\"")

(deffailure-var-test
 "Examples - doesn't have arrow"
 "*UNUSED*

VALUE TYPE:

  a generalized boolean

INITIAL VALUE:

  NIL

DESCRIPTION:

  This is a mock description.

EXAMPLES:

  *unused* - :success
"
 "Example line does not match \"  example => result\":   *unused* - :success")

(deffailure-var-test
 "Examples - doesn't start with EXAMPLES"
 "*UNUSED*

VALUE TYPE:

  a generalized boolean

INITIAL VALUE:

  NIL

DESCRIPTION:

  This is a mock description.

EXAAMPLES:

  *unused* => :success"
 "Got unexpected line, requires blank lines or start with two spaces: \"EXAAMPLES:\"")

(deffailure-var-test
 "Header - first line doesn't start with var-name (naturally all in upper case)"
 "*UNUUSED*

INITIAL VALUE:

  RESULT: a pathname

DESCRIPTION:

"
 "First line of *UNUSED* did not match: *UNUSED*, *UNUUSED*")

(deffailure-var-test
 "General - No value type"
 "*UNUSED*

INITIAL VALUE:

  NIL

DESCRIPTAION:

"
 "Expected VALUE TYPE: instead of: INITIAL VALUE:")

(deffailure-var-test
 "General - No initial value"
 "*UNUSED*

VALUE TYPE:

  a generalized boolean

DESCRIPTION:

"
 "Got unexpected line, requires blank lines or start with two spaces: \"DESCRIPTION:\"")

(deffailure-var-test
 "General - Ends early"
 "*UNUSED*

VALUE TYPE:

  a generalized boolean"
 "Got unexpected line, requires blank lines or start with two spaces: NIL")
