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
; If a same symbol exists in :variable and :symbol-macro, a program-error will be signaled.
#?(augment-environment nil :variable '(a) :symbol-macro '((a :def))) :signals program-error
; If a symbol-macro also specified as special, a program-error will be signaled.
#?(augment-environment nil :symbol-macro '((a :def)) :declare '((special a)))
:signals program-error
; If a function also specified as macro, a program-error will be signaled.
#?(augment-environment nil :function '(a) :macro `((a ,#'car)))
:signals program-error

(requirements-about VARIABLE-INFORMATION :doc-type function)

;;;; Description:

#+syntax (VARIABLE-INFORMATION VAR-NAME &OPTIONAL ENV) ; => result

;;;; Arguments and Values:

; var-name := symbol, otherwise implementation dependent condition will be signaled.
#?(variable-information "not symbol") :signals condition

; env := (or null environment), otherwise implementation dependent condition will be signaled.
#?(variable-information 'symbol "not env") :signals condition

; result 1 := (member :constant :symbol-macro :lexical :special nil)
; When VAR-NAME names constant, :CONSTANT will be returned.
#?(variable-information 'pi) => :constant
; When VAR-NAME is keyword symbol, :CONSTANT will be returned.
#?(variable-information :keyword) => :constant
; When VAR-NAME is specified as symbol-macro, :SYMBOL-MACRO will be returned.
#?(variable-information 'symbol-macro
			(augment-environment nil :symbol-macro '((symbol-macro :def))))
=> :SYMBOL-MACRO
; When VAR-NAME is specified as variable lexically, :lexical will be returned.
#?(variable-information 'lexical
			(augment-environment nil :variable '(lexical)))
=> :LEXICAL
; When VAR-NAME is specified as special, :special will be returned.
#?(variable-information 'var
			(augment-environment nil :variable '(var)
					     :declare '((special var))))
=> :SPECIAL
; Otherwise NIL is returned.
#?(variable-information 'var) => NIL

; result 2 := boolean
; When VAR-NAME can be refered in the ENVIRONMENT, true will be returned.
#?(nth-value 1 (variable-information 'var
				     (augment-environment nil :variable '(var))))
=> T
; Otherwise NIL is returned.
#?(nth-value 1 (variable-information 'var)) => NIL

; result 3 := list
; When VAR-NAME has declarations, such declarations are returned.
#?(nth-value 2 (variable-information 'var)) => NIL
#?(nth-value 2 (variable-information 'var
				     (augment-environment nil :variable '(var))))
=> NIL
#?(nth-value 2 (variable-information 'var
				     (augment-environment nil :variable '(var)
							  :declare '((type fixnum var)))))
=> ((TYPE . FIXNUM))
,:test equal

;;;; Affected By:
; none

;;;; Side-Effects:
; none

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

