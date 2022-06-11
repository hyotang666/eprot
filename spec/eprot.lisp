(defpackage :eprot.spec
  (:use :cl :jingoh :eprot)
  (:shadowing-import-from :eprot macro-function macroexpand-1 macroexpand *macroexpand-hook*))
(in-package :eprot.spec)
(setup :eprot)

(requirements-about AUGMENT-ENVIRONMENT :doc-type function)

;;;; Description:

#+syntax (AUGMENT-ENVIRONMENT ENV
           &KEY
           VARIABLE
           SYMBOL-MACRO
           FUNCTION
           MACRO
           DECLARE)
; => result

;;;; Arguments and Values:

; env := (or null environment), otherwise implementation dependent condition.
#?(augment-environment :not-env) :signals condition

; variable := list, otherwise implementation dependent condition.
#?(augment-environment nil :variable :not-list) :signals condition
; Each element must be a symbol, otherwise an error is signaled.
#?(augment-environment nil :variable '("not symbol")) :signals error

; symbol-macro := list, otherwise implementation dependent condition.
#?(augment-environment nil :variable :not-list) :signals condition
; Each element must be (symbol expression), otherwise an error is signaled.
#?(augment-environment nil :symbol-macro '("not cons")) :signals error
#?(augment-environment nil :symbol-macro '((less) (elt))) :signals error
#?(augment-environment nil :symbol-macro '((too much elt))) :signals error
#?(augment-environment nil :symbol-macro '(("not symbol" :dummy))) :signals error
#?(augment-environment nil :symbol-macro '((fine :definition)))
:be-the environment

; function := list, otherwise implementation dependent condition.
#?(augment-environment nil :variable :not-list) :signals condition
; Each element must be a symbol, otherwise an error is signaled.
#?(augment-environment nil :function '("not symbol")) :signals error

; macro := list, otherwise implementation dependent condition.
#?(augment-environment nil :macro :not-list) :signals condition
; Each element must be a (symbol function), otherwise an error is signaled.
#?(augment-environment nil :macro '("not cons")) :signals error
#?(augment-environment nil :macro '((lee) (elt))) :signals error
#?(augment-environment nil :macro '((too much elt))) :signals error
#?(augment-environment nil :macro '((too much elt))) :signals error
#?(augment-environment nil :macro '(("not symbol" :dummy))) :signals error
#?(augment-environment nil :macro '((fine "but not function"))) :signals error
#?(augment-environment nil :macro `((fine ,#'car)))
:be-the environment

; declare := list, otherwise implementation dependent condition.
#?(augment-environment nil :declare :not-list) :signals condition
; Each element must be a (symbol ...), otherwise an error is signaled.
#?(augment-environment nil :declare '("not cons")) :signals condition
#?(augment-environment nil :declare '(("not symbol"))) :signals condition
#?(augment-environment nil :declare '((fine)))
:be-the environment

; result := environment
#?(augment-environment nil) :be-the environment

;;;; Affected By:
; none.

;;;; Side-Effects:
; none.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about MACRO-FUNCTION :doc-type function)

;;;; Description:

#+syntax (MACRO-FUNCTION SYMBOL &OPTIONAL ENVIRONMENT) ; => result

;;;; Arguments and Values:

; symbol := 

; environment := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about MACROEXPAND-1 :doc-type function)

;;;; Description:

#+syntax (MACROEXPAND-1 FORM &OPTIONAL ENVIRONMENT) ; => result

;;;; Arguments and Values:

; form := 

; environment := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about MACROEXPAND :doc-type function)

;;;; Description:

#+syntax (MACROEXPAND FORM &OPTIONAL ENVIRONMENT) ; => result

;;;; Arguments and Values:

; form := 

; environment := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about *MACROEXPAND-HOOK* :doc-type variable)

;;;; Description:

;;;; Value type is SYMBOL
;#? *MACROEXPAND-HOOK* :be-the ???

; Initial value is `FUNCALL`

;;;; Affected By:

;;;; Notes:

(requirements-about VARIABLE-INFORMATION :doc-type function)

;;;; Description:

#+syntax (VARIABLE-INFORMATION VAR-NAME &OPTIONAL ENV) ; => result

;;;; Arguments and Values:

; var-name := symbol

; env := (or null environment)

; result 1 := (member :constant :symbol-macro :lexical :special nil)

; result 2 := boolean

; result 3 := list

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about FUNCTION-INFORMATION :doc-type function)

;;;; Description:

#+syntax (FUNCTION-INFORMATION FUN-NAME &OPTIONAL ENV) ; => result

;;;; Arguments and Values:

; fun-name := symbol

; env := (or null environment)

; result 1 := (member :special-form :macro :function nil)

; result 2 := boolean

; result 3 := list

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about DECLARATION-INFORMATION :doc-type function)

;;;; Description:

#+syntax (DECLARATION-INFORMATION DECL-NAME &OPTIONAL ENV) ; => result

;;;; Arguments and Values:

; decl-name := symbol

; env := (or null environment)

; result := t

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about PARSE-MACRO :doc-type function)

;;;; Description:

#+syntax (PARSE-MACRO NAME LAMBDA-LIST BODY &OPTIONAL ENV) ; => result

;;;; Arguments and Values:

; name := symbol

; lambda-list := list

; body := list

; env := (or null environment)

; result := list

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ENCLOSE :doc-type function)

;;;; Description:

#+syntax (ENCLOSE LAMBDA-EXPRESSION &OPTIONAL ENV) ; => result

;;;; Arguments and Values:

; lambda-expression := list

; env := (or null environment)

; result := function

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about *ENVIRONMENT* :doc-type variable)

;;;; Description:

;;;; Value type is ENVIRONMENT
;#? *ENVIRONMENT* :be-the ???

; Initial value is `:UNBOUND`

;;;; Affected By:

;;;; Notes:

