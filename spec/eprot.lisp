(defpackage :eprot.spec
  (:use :cl :jingoh :eprot)
  (:shadowing-import-from :eprot macro-function macroexpand-1 macroexpand *macroexpand-hook* proclaim list-all-declarations))
(in-package :eprot.spec)
(setup :eprot)

(requirements-about DECLARATION-IDENTIFIER :doc-type type)
;;;; Description:
; Any symbol can be a declaration-identifier.
#?(typep 'any 'declaration-identifier) => T

#+syntax
(declaration-identifier)

(requirements-about DECLARATION-IDENTIFIER-P :doc-type function)

;;;; Description:

#+syntax (DECLARATION-IDENTIFIER-P SYMBOL) ; => result

;;;; Arguments and Values:

; symbol := Symbol that names declaration-identifier, otherwise nil.
#?(declaration-identifier-p "not symbol") => NIL

; result := boolean, be true when the SYMBOL is named as declaration-specifier, otherwise nil.
#?(declaration-identifier-p 'special) => T
#?(declaration-identifier-p 'no-such) => NIL

;;;; Affected By:
; Global declaration identifier database, i.e. eprot::*declaration-specifiers*.

;;;; Side-Effects:
; none

;;;; Notes:
; To define new declaration-identifier, see define-declaration, proclaim, and optionally define-declaration-specifier.

;;;; Exceptional-Situations:

(requirements-about ENVIRONMENT :doc-type STRUCTURE)

;;;; Description:
;;;; Class Precedence List: (case in SBCL)
; environment structure-object slot-object t

;;;; Effective Slots:

; NAME [Type] T

; VARIABLE [Type] LIST

; SYMBOL-MACRO [Type] LIST

; FUNCTION [Type] LIST

; MACRO [Type] LIST

; DECLARE [Type] LIST

; NEXT [Type] (OR NULL ENVIRONMENT)

; DECLARATION-HANDLERS [Type] HASH-TABLE

;;;; Notes:

(requirements-about FIND-ENVIRONMENT :doc-type function)

;;;; Description:

#+syntax (FIND-ENVIRONMENT ENV-NAME &OPTIONAL (ERRORP T)) ; => result

;;;; Arguments and Values:

; env-name := symbol, otherwise implementation dependent condition is signaled.
#?(find-environment "not symbol") :signals condition

; errorp := boolean, specify signal an error when ENV-NAME is missing. (the default is T.)
#?(find-environment :no-such) :signals missing-environment
#?(find-environment :no-such nil) => NIL

; result := (or null environment)
#?(find-environment :standard) :be-the environment

;;;; Affected By:
; eprot::*environments*

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about *ENVIRONMENT* :doc-type variable)

;;;; Description:

;;;; Value type is ENVIRONMENT
#? *ENVIRONMENT* :be-the environment

; Initial value is :standard environment.
#?*environment* :equivalents (find-environment :standard)

;;;; Affected By:
; in-environment

;;;; Notes:

(requirements-about IN-ENVIRONMENT :doc-type function)

;;;; Description:

#+syntax (IN-ENVIRONMENT ENV-NAME) ; => result

;;;; Arguments and Values:

; env-name := symbol, otherwise implementation dependent condition is signaled.
#?(in-environment "not symbol") :signals condition

; result := ENVIRONMENT
#?(in-environment :standard) :be-the environment

;;;; Affected By:
; eprot::*environments*

;;;; Side-Effects:
; Modify *environment*.

;;;; Notes:

;;;; Exceptional-Situations:
; When specified environment are not found, an error is signaled.
#?(in-environment :no-such) :signals missing-environment

(requirements-about DEFENV :doc-type function)

;;;; Description:

#+syntax (DEFENV ENV-NAME &KEY VARIABLE SYMBOL-MACRO FUNCTION MACRO DECLARE)
; => result

;;;; Arguments and Values:

; env-name := symbol, otherwise implementation dependent condition is signaled.
#?(defenv "not symbol") :signals condition

; variable := The form which generates list of symbols.

; symbol-macro := The form which genarates list of (symbol t).

; function := The form which generates list of symbols.

; macro := the form which generates list of (symbol function).

; declare := the form which generates list of (decl-name spec*).

; result := ENV-NAME

;;;; Affected By:
; eprot::*environments*

;;;; Side-Effects:
; Modify eprot::*environments*

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about DECL-SPEC :doc-type STRUCTURE)

;;;; Description:
;;;; Class Precedence List: (case in SBCL)
; decl-spec structure-object slot-object t

;;;; Effective Slots:

; TYPE [Type] (MEMBER :VARIABLE :FUNCTION :DECLARE :BIND)

; INFO [Type] LIST

;;;; Notes:

(requirements-about PARSE-DECLARATION-SPEC :doc-type function)

;;;; Description:

#+syntax (PARSE-DECLARATION-SPEC DECL-SPEC &OPTIONAL ENV) ; => result

;;;; Arguments and Values:

; decl-spec := (decl-name &rest spec), otherwise an error is signaled.
#?(parse-declaration-spec :not-list) :signals error

; env := (or null environment), otherwise an error is signaled.
#?(parse-declaration-spec () "not env") :signals error

; result := decl-spec.
#?(parse-declaration-spec '(type fixnum a)) :be-the decl-spec

;;;; Affected By:
; *environment*

;;;; Side-Effects:
; ENV may destructively modified depending on declaration handler.

;;;; Notes:

;;;; Exceptional-Situations:
; When decl-name is not known in ENV, an error is signaled.
#?(parse-declaration-spec '(unknown spec)) :signals unknown-declaration

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
#?(augment-environment nil :declare '((inline)))
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
; If the declaration spec does not match its spec, an error is signaled.
#?(augment-environment nil :declare '((type)))
:signals bad-declaration-specifier

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
#?(variable-information 'most-positive-fixnum) => :constant
; When VAR-NAME is keyword symbol, :CONSTANT will be returned.
#?(variable-information :keyword) => :constant
; When VAR-NAME is specified as symbol-macro, :SYMBOL-MACRO will be returned.
#?(variable-information 'symbol-macro
			(augment-environment nil :symbol-macro '((symbol-macro :def))))
=> :SYMBOL-MACRO
; When the environment is a toplevel one, i.e. the first argument is NIL or :standard environment,
; such environment is treated as global one.
#?(variable-information 'lexical
			(augment-environment nil :variable '(lexical)))
=> NIL ; because it is global, not lexical one.
; When VAR-NAME is specified as variable lexically, :lexical will be returned.
#?(variable-information 'lexical
			(augment-environment (augment-environment nil)
					     :variable '(lexical)))
=> :LEXICAL
; When VAR-NAME is specified as special, :special will be returned.
#?(variable-information 'var
			(augment-environment nil :variable '(var)
					     :declare '((special var))))
=> :SPECIAL
; Otherwise NIL is returned.
#?(variable-information 'var) => NIL

; result 2 := boolean
; When VAR-NAME can be refered as lexical one in the ENVIRONMENT, true will be returned.
#?(nth-value 1 (variable-information 'var
				     (augment-environment nil :variable '(var))))
=> NIL ; Because it is global one.
#?(nth-value 1 (variable-information 'var
				     (augment-environment (augment-environment nil)
							  :variable '(var))))
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

(requirements-about FUNCTION-INFORMATION :doc-type function)

;;;; Description:

#+syntax (FUNCTION-INFORMATION FUN-NAME &OPTIONAL ENV) ; => result

;;;; Arguments and Values:

; fun-name := symbol, otherwise implementation dependent condition will be signaled.
#?(function-information "not symbol") :signals condition

; env := (or null environment), otherwise implementation dependent condition will be signaled.
#?(function-information 'dummy "not env") :signals condition

; result 1 := (member :special-form :macro :function nil)
; TODO case :special-form.
; If FUN-NAME is specified as macro, :MACRO will be returned.
; Case in the toplevel environment.
#?(function-information 'name
			(augment-environment nil :macro `((name ,#'car))))
:values (:MACRO NIL NIL)
; Case in the lexical environment.
#?(function-information 'name
			(augment-environment (augment-environment nil)
					     :macro `((name ,#'car))))
:values (:MACRO T NIL)
#?(function-information 'name
			(augment-environment nil :macro `((name ,#'car))))
:values (:MACRO NIL NIL)
; If FUN-NAME is specified as function, :FUNCTION will be returned.
; Case in the toplevel environment.
#?(function-information 'name
			(augment-environment nil :function '(name)))
:values (:FUNCTION NIL NIL)
; Case in the lexical environment.
#?(function-information 'name
			(augment-environment (augment-environment nil)
					     :function '(name)))
:values (:FUNCTION T NIL)

; Otherwise NIL is returned.
#?(function-information 'name) :values (NIL NIL NIL)

; result 2 := boolean, when FUN-NAME exists in ENVIRONMENT as function, true. Otherwise NIL.

; result 3 := list
; If FUN-NAME has declarations, such declarations are returned.
; Case in the toplevel environment.
#?(function-information 'name
			(augment-environment nil :function '(name)
					     :declare '((ftype (function * fixnum) name))))
:values (:FUNCTION NIL ((FTYPE . (FUNCTION * FIXNUM))))
; Case in the lexical environment.
#?(function-information 'name
			(augment-environment (augment-environment nil)
					     :function '(name)
					     :declare '((ftype (function * fixnum) name))))
:values (:FUNCTION T ((FTYPE . (FUNCTION * FIXNUM))))

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about DECLARATION-INFORMATION :doc-type function)

;;;; Description:

#+syntax (DECLARATION-INFORMATION DECL-NAME &OPTIONAL ENV) ; => result

;;;; Arguments and Values:

; decl-name := symbol, otherwise implementation dependent condition will be signaled.
#?(declaration-information "not symbol") :signals condition

; env := (or null environment), otherwise implementation dependent condition will be signaled.
#?(declaration-information nil "not env") :signals condition

; result := t

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about PARSE-MACRO :doc-type function)

;;;; Description:

#+syntax (PARSE-MACRO NAME LAMBDA-LIST BODY &OPTIONAL ENV) ; => result

#?(parse-macro 'name '(&whole w a &environment e) '(`(print ',a)))
=> (lambda (w e)
     (destructuring-bind (a) (cdr w)
       (block name
       `(print ',a))))
,:test equal

;;;; Arguments and Values:

; name := symbol, otherwise implementation dependent condition will be signaled.
#?(parse-macro "not symbol" () ()) :signals condition

; lambda-list := list, otherwise implementation dependent condition will be signaled.
#?(parse-macro 'dummy :not-list ()) :signals condition

; body := list, otherwise implementation dependent condition will be signaled.
#?(parse-macro 'dummy () :not-list) :signals condition

; env := (or null environment), otherwise implementation dependent condition will be signaled.
#?(parse-macro 'dummy () () "not env") :signals condition

; result := list as lambda expression.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ENCLOSE :doc-type function)

;;;; Description:

#+syntax (ENCLOSE LAMBDA-EXPRESSION &OPTIONAL ENV) ; => result

;;;; Arguments and Values:

; lambda-expression := list, otherwise implementation dependent condition will be signaled.
#?(enclose :not-list) :signals condition

; env := (or null environment), otherwise implementation dependent condition will be signaled.
#?(enclose '(lambda ()) "not env") :signals condition

; result := function

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

; symbol := symbol, otherwise implementation dependent condition will be signaled.
#?(macro-function "not-symbol") :signals condition
; If SYMBOL is specified as a macro in the ENVIRONMENT, its expand function is returned.
#?(macro-function 'name (augment-environment nil
					     :macro `((name ,#'car))))
:equivalents #'car
; otherwise NIL.
#?(macro-function 'name) => NIL

; environment := (or null environment), otherwise implementation dependent condition will be signaled.
#?(macro-function 'dummy "not env") :signals condition

; result := (or null function)

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about *MACROEXPAND-HOOK* :doc-type variable)

;;;; Description:

;;;; Value type is (or symbol function)
#? *MACROEXPAND-HOOK* :be-the (or symbol function)

; Initial value is `FUNCALL`

;;;; Affected By:

;;;; Notes:

(requirements-about MACROEXPAND-1 :doc-type function)

;;;; Description:

#+syntax (MACROEXPAND-1 FORM &OPTIONAL ENVIRONMENT) ; => result

;;;; Arguments and Values:

; form := t

; environment := (or null environment), otherwise implementation dependent condition will be signaled.
#?(macroexpand-1 t "not env") :signals condition

; result 1 := t

; result 2 := boolean

; If the FORM is a macro-call-form, the expanded form and T is returned.
#?(macroexpand-1 '(macro)
		 (augment-environment nil
				       :macro (list (list 'macro
							  (enclose
							    (parse-macro 'macro () '('hoge)))))))
:values (HOGE T)

; Otherwise the FORM and NIL is returned.
#?(macroexpand-1 '(macro)) :values ((MACRO) NIL)

;;;; Affected By:
; *MACROEXPAND-HOOK*
#?(let ((*macroexpand-hook*
	  (lambda (expander form env)
	    (declare (ignore expander form env))
	    :this-is-returned-as-expanded-result)))
    (macroexpand-1 '(macro)
		   (augment-environment nil
					:macro (list (list 'macro
							   (enclose
							     (parse-macro 'macro () '('hoge))))))))
:values (:THIS-IS-RETURNED-AS-EXPANDED-RESULT T)

;;;; Side-Effects:
; none

;;;; Notes:
; Symbol macro is also treated.
#?(macroexpand-1 'symbol-macro
		 (augment-environment nil
				      :symbol-macro '((symbol-macro :expanded))))
:values (:EXPANDED T)

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

(requirements-about LIST-ALL-DECLARATIONS :doc-type function)

;;;; Description:

#+syntax (LIST-ALL-DECLARATIONS &OPTIONAL (ENV *ENVIRONMENT*)) ; => result

;;;; Arguments and Values:

; env := environment, otherwise an error is signaled.
#?(list-all-declarations "not env") :signals error

; result := The list of the decl-names.
#?(list-all-declarations (find-environment :standard))
:satisfies
(lambda (list)
  (& (null (set-difference list '(special type ftype inline notinline optimize declaration dynamic-extent ignore ignorable)))))

;;;; Affected By:
; *environment* when optional arg is not specified.

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about PROCLAIM :doc-type function)

;;;; Description:

#+syntax (PROCLAIM DECL-SPEC) ; => result

;;;; Arguments and Values:

; decl-spec := (decl-name . args), otherwise an error is signaled.
#?(proclaim :not-list) :signals error

; result := implementation dependent.
#?(let ((*environment* (augment-environment nil)))
    (proclaim '(type fixnum a)))
=> implementation-dependent

;;;; Affected By:

;;;; Side-Effects:
; Destructively modify current environment, i.e. *environment*.
; Case global one.
#?(let ((*environment* (augment-environment nil)))
    (proclaim '(type fixnum a))
    (variable-information 'a))
:values (NIL NIL ((TYPE . FIXNUM)))
; Case lexical non-null one.
#?(let ((*environment* (augment-environment *environment*)))
    (proclaim '(type fixnum a))
    (variable-information 'a *environment*))
:values (:LEXICAL T ((TYPE . FIXNUM)))
; NOTE: Pitfalls.
#?(let ((*environment* (augment-environment *environment*))) ; <--- Non null env.
    (proclaim '(type fixnum a)) ; Since non-null env, this declaration becomes local one.
    (variable-information 'a)) ; <--- Null lexical env, in other words, specify global info.
:values (NIL NIL NIL)

;;;; Notes:

;;;; Exceptional-Situations:
; If DECL-NAME is unknown, an error is signaled.
#?(proclaim '(no-such declaration)) :signals unknown-declaration

(requirements-about DECL-SPEC-ASSERTION :doc-type function)

;;;; Description:
; Internal use.

#+syntax (DECL-SPEC-ASSERTION SPEC+) ; => result

;;;; Arguments and Values:

; spec+ := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests.
; The case when too long requirement args.
#?(funcall (eprot::decl-spec-assertion '(symbol)) '(id too long))
:signals bad-declaration-specifier
; The case unknown key comes.
#?(funcall (eprot::decl-spec-assertion '(&key (:known symbol))) '(id :unknown :key))
:signals bad-declaration-specifier
; The case unknown key comes but specifid :allow-other-keys t
#?(funcall (eprot::decl-spec-assertion '(&key (:known symbol))) '(id :unknown :key :allow-other-keys t))
=> NIL
; The case unknown key comes but specifid &allow-other-keys
#?(funcall (eprot::decl-spec-assertion '(&key (:known symbol) &allow-other-keys)) '(id :unknown :key))
=> NIL
; The case when required args are too short.
#?(funcall (eprot::decl-spec-assertion '(symbol)) '(id))
:signals bad-declaration-specifier
; The case type mismatch in required arguments.
#?(funcall (eprot::decl-spec-assertion '(symbol)) '(id "not symbol"))
:signals bad-declaration-specifier
; The case type mismatch in optional arguments.
#?(funcall (eprot::decl-spec-assertion '(&optional symbol)) '(id "not symbol"))
:signals bad-declaration-specifier
; The case type mismatch in rest arguments.
#?(funcall (eprot::decl-spec-assertion '(&rest symbol)) '(id "not symbol"))
:signals bad-declaration-specifier
; The case type mismatch in keyword arguments.
#?(funcall (eprot::decl-spec-assertion '(&key (:name symbol))) '(id :name "not symbol"))
:signals bad-declaration-specifier
