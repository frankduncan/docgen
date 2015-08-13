# Package SUCCESS1

This defines a simple successful package.

This is should all get pulled in and the markdown.md should be equal to success1.md.

## Function **FUNC-THAT-DOES-STUFF**

#### Syntax:

**func-that-does-stuff** _path_ _x_ => _result_

```result::= success-result | failure-result```  
```success-result::= (:success filename)```  
```failure-result::= (:failure filename msg)```  

#### Arguments and Values:

_path_---a pathname  
_x_---a random value related to _path_  
_filename_---the file this func was run on  
_msg_---a string containing the failure message  

#### Description:

_func-that-does-stuff_ runs all the things against a file and returns as soon as the first func error is found.

This second section uses _path_ and _x_ as something we should talk about, but doesn't use all the arguments (let's include _path_ here for fun)

#### Examples:

```(func-that-does-stuff #P"path/to/file.lisp" t)``` => ```(:success "path/to/file.lisp")```  
```(func-that-does-stuff #P"path/to/error.lisp" nil)``` => ```(:failure "path/to/error.lisp" "Error msg" 20 0)```  

## Function **HAS-KEYWORDS**

#### Syntax:

**has-keywords** _path_ _&key_ _x_ => _result_

```result::= success-result | failure-result```  
```success-result::= (:success filename)```  
```failure-result::= (:failure filename msg)```  

#### Arguments and Values:

_path_---a pathname  
_x_---a random value related to _path_  
_filename_---the file this func was run on  
_msg_---a string containing the failure message  

#### Description:

_has-keywords_ runs all the things against a file and returns as soon as the first func error is found.

This second section uses _path_ and _x_ as something we should talk about, but doesn't use all the arguments (let's include _path_ here for fun)

## Function **HAS-NO-EXAMPLES**

#### Syntax:

**has-no-examples** => _result_

```result::= success-result | failure-result```  
```success-result::= (:success filename)```  
```failure-result::= (:failure filename msg)```  

#### Arguments and Values:

_filename_---the file this func was run on  
_msg_---a string containing the failure message  

#### Description:

_has-no-examples_ runs all the things against a file and returns as soon as the first func error is found.

## Function **HAS-OPTIONAL**

#### Syntax:

**has-optional** _path_ _&optional_ _x_ => _result_

```result::= success-result | failure-result```  
```success-result::= (:success filename)```  
```failure-result::= (:failure filename msg)```  

#### Arguments and Values:

_path_---a pathname  
_x_---a random value related to _path_  
_filename_---the file this func was run on  
_msg_---a string containing the failure message  

#### Description:

_has-optional_ runs all the things against a file and returns as soon as the first func error is found.

This second section uses _path_ and _x_ as something we should talk about, but doesn't use all the arguments (let's include _path_ here for fun)

## Function **HAS-REST**

#### Syntax:

**has-rest** _path_ _&rest_ _x_ => _result_

```result::= success-result | failure-result```  
```success-result::= (:success filename)```  
```failure-result::= (:failure filename msg)```  

#### Arguments and Values:

_path_---a pathname  
_x_---a random value related to _path_  
_filename_---the file this func was run on  
_msg_---a string containing the failure message  

#### Description:

_has-rest_ runs all the things against a file and returns as soon as the first func error is found.

This second section uses _path_ and _x_ as something we should talk about, but doesn't use all the arguments (let's include _path_ here for fun)

## Function **NOARGS**

#### Syntax:

**noargs** => _result_

```result::= success-result | failure-result```  
```success-result::= (:success filename)```  
```failure-result::= (:failure filename msg)```  

#### Arguments and Values:

_filename_---the file this func was run on  
_msg_---a string containing the failure message  

#### Description:

_noargs_ runs all the things against a file and returns as soon as the first func error is found.

#### Examples:

```(func-that-does-stuff)``` => ```(:success "path/to/file.lisp")```  
```(func-that-does-stuff)``` => ```(:failure "path/to/error.lisp" "Error msg" 20 0)```  

## Function **RESULT-LIST**

#### Syntax:

**result-list** => _result_

```result::= failure-result*```  
```failure-result::= (:failure filename msg)```  

#### Arguments and Values:

_filename_---the file this func was run on  
_msg_---a string containing the failure message  

#### Description:

NOARGS runs all the things against a file and returns as soon as the first func error is found.

## Function **VALUES-RESULT**

#### Syntax:

**values-result** => _result1_, _result2_, _result3_

```result1::= success-result | failure-result```  
```success-result::= (:success filename)```  
```failure-result::= (:failure filename msg)```  

#### Arguments and Values:

_result2_---second result  
_result3_---third result  
_filename_---the file this func was run on  
_msg_---a string containing the failure message  

#### Description:

_values-result_ runs all the things against a file and returns as soon as the first func error is found.
