(in-package :cl-user)

(defpackage :eprot
  (:use :cl)
  (:shadow .
           #0=(macro-function macroexpand-1 macroexpand *macroexpand-hook*
                              proclaim))
  (:export ; cl things.
           .
           #0#)
  (:export ; cltl2 things.
           #:augment-environment
           #:variable-information
           #:function-information
           #:declaration-information
           #:parse-macro
           #:enclose
           #:define-declaration)
  (:export ; EPROT things.
           ;;;; CONDITION
           #:eprot-error
           #:unknown-declaration
           #:missing-environment
           #:bad-declaration-specifier
           ;;;; ENVIRONMENT
           #:environment
           #:*environment*
           #:find-environment
           #:defenv
           #:in-environment
           #:list-all-environments
           ;;;; DECL-SPEC
           #:decl-spec ; object
           #:parse-declaration-spec ; constructor
           ;; reader
           #:decl-spec-type
           #:decl-spec-info))

(in-package :eprot)

(declaim (optimize speed))

;;;; TODO
#|
* defmacro
* flet
* labels
* let
* let*
* macrolet
* symbol-macrolet
* define-compiler-macro
* compiler-macro-function
* compiler-macroexpand-1
* compiler-macroexpand
* declaim
* defun
* defconstant
* defvar
* defparameter
|#

;;;; CONDITIONS.

(define-condition eprot-error (error) ())

(define-condition simple-program-error (eprot-error program-error simple-condition)
  ())

(defun did-you-mean (out name sellection)
  #+sbcl ; Formatter this. Out of our responsibility.
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (apply (formatter "Did you mean ~#[~;~S~;~S or ~S~:;~S, ~S, or ~S~] ?") out
         (fuzzy-match:fuzzy-match (symbol-name name) sellection)))

;;;; ENVIRONMENT OBJECT
#| CLtL2 quotes:
 | In all of these functions the argument named env is an environment object.
 | Optional env arguments default to nil,
 | which represents the local null lexical environment (containing only global definitions and proclamations that are present in the run-time environment).
 |#

#| EPROT NOTE:
 | In order to provide some environments for one lisp implementation,
 | in EPROT design, the NIL itself does not represent the local null lexical environment.
 | The environment that NEXT slot is NIL represents the local null lexical environment.
 | Additionally, we provide the CL:PACKAGE like features, in other words,
 | we provide a special variable *ENVIRONMNET* which represents the current environment.
 | Hence, the NIL represents the current null lexical environment in EPROT.
 |#

(defstruct environment
  (name nil)
  (variable nil :type list)
  (symbol-macro nil :type list)
  (function nil :type list)
  (macro nil :type list)
  (declare nil :type list)
  ;; The pointer to point the parent environment.
  ;; If this is NIL, such an environment is a null lexical environment.
  ;; NOTE: Our ENVIRONMENTs are designed as the stack-like.
  (next nil :type (or null environment) :read-only t)
  ;; Only the null lexical environment has the table, otherwise nil.
  (declaration-handlers nil :type (or null hash-table) :read-only t))

(defmethod print-object ((o environment) output)
  (cond (*print-readably* (call-next-method))
        (t
         (print-unreadable-object (o output :type t :identity t)
           (write (or (environment-name o) "Anonymous") :stream output)))))

;;;; DECL-SPEC

(defstruct (decl-spec (:constructor make-decl-spec (type info)))
  (type (error "TYPE is required.")
        :type (member :variable :function :declare :bind)
        :read-only t)
  (info nil :type list :read-only t))

;;;; TYPES

(deftype var-name () 'symbol)

(deftype variable-type () '(member :special :lexical :symbol-macro :constant))

(deftype declaration-alist () 'list)

(deftype function-name () 'symbol)

(deftype function-type () '(member :function :macro :special-form))

(deftype lexicalp () 'boolean)

(deftype declaration-name () 'symbol)

(deftype macro-name () 'symbol)

(deftype lambda-list () 'list)

(deftype body () 'list)

(deftype lambda-expression () 'list)

(deftype env-name () 'symbol)

;;;; FTYPES

(declaim ;; Named environment features.
         (ftype (function (env-name &optional boolean)
                 (values (or null environment) &optional))
                find-environment)
         ;; CLtL2 features.
         (ftype (function (var-name &optional (or null environment))
                 (values (or null variable-type)
                         lexicalp
                         declaration-alist
                         &optional))
                variable-information)
         (ftype (function (function-name &optional (or null environment))
                 (values (or null function-type)
                         lexicalp
                         declaration-alist
                         &optional))
                function-information)
         (ftype (function (declaration-name &optional (or null environment))
                 (values t &optional))
                declaration-information)
         (ftype (function
                 ((or null environment) &key (:variable list)
                  (:symbol-macro list) (:function list) (:macro list)
                  (:declare list) (:name t))
                 (values environment &optional))
                augment-environment)
         (ftype (function
                 (macro-name lambda-list body &optional (or null environment))
                 (values lambda-expression &optional))
                parse-macro)
         (ftype (function (lambda-expression &optional (or null environment))
                 (values function &optional))
                enclose)
         ;; CL Features.
         (ftype (function (symbol &optional (or null environment))
                 (values (or null function) &optional))
                macro-function)
         (ftype (function (t &optional (or null environment))
                 (values t boolean &optional))
                macroexpand-1))

;;;; ITERATOR

(defmacro do-env ((var <environment> &optional <return>) &body body)
  `(do ((,var ,<environment> (environment-next ,var)))
       ((null ,var) ,<return>)
    ,@body))

;;;; SPECIAL VARIABLE

(defvar *environments* (make-hash-table :test #'eq))

(define-condition missing-environment (eprot-error cell-error)
  ()
  (:report
   (lambda (this out)
     (format out "Missing environment ~S." (cell-error-name this)))))

(defun list-all-environments () (alexandria:hash-table-keys *environments*))

(defun find-environment (env-name &optional (errorp t))
  (or (gethash env-name *environments*)
      (and errorp (error 'missing-environment :name env-name))))

(defun store-environment (env-name env)
  (setf (gethash env-name *environments*) env))

(defvar *environment*)

(declaim (environment *environment*))

(defmacro in-environment (env-name)
  (check-type env-name symbol)
  `(setf *environment* (find-environment ',env-name)))

(defmacro defenv
          (env-name
           &key variable symbol-macro function macro declare handler
           (use :standard))
  (check-type env-name symbol)
  `(let ((*environment*
          ,(case use
             ((nil)
              `(make-environment :declaration-handlers (make-hash-table :test #'eq)))
             (:standard `(copy-env nil))
             (otherwise `(copy-env (find-environment ',use))))))
     ,@(mapcar (lambda (definition) `(define-declaration ,@definition))
               handler)
     (store-environment ',env-name
                        (augment-environment *environment*
                                             :variable ,variable
                                             :symbol-macro ,symbol-macro
                                             :function ,function
                                             :macro ,macro
                                             :declare ,declare
                                             :name ',env-name))
     ',env-name))

(defun pprint-defenv (out exp &rest noise)
  (declare (ignore noise))
  (funcall (formatter "~:<~W~^ ~1I~:_~W~^ ~_~@{~W~^ ~:_~W~^ ~_~}~:>") out exp))

(set-pprint-dispatch '(cons (member defenv)) 'pprint-defenv)

(store-environment :standard (make-environment :name :standard
                                               :declaration-handlers (make-hash-table
                                                                       :test #'eq)))

(in-environment :standard)

;;;; MISCELLANEOUS

(defun null-lexical-environment (&optional env)
  "Return the null lexical environment of the ENV.
If ENV is NIL, the current environment's one is returned."
  (do-env (e (or env *environment*))
    (when (null (environment-next e))
      (return e))))

(defun declaration-handlers (&optional env)
  "Return the declaration handlers table of the ENV.
If ENV is NIL, the current environment's one is returned."
  (environment-declaration-handlers (null-lexical-environment env)))

(defun copy-env (&optional env)
  "Return the fresh copied environment of the ENV.
If ENV is NIL, the current null lexical environment's one is returned."
  (with-slots (name variable symbol-macro function macro declare next
               declaration-handlers)
      (or env (null-lexical-environment))
    (make-environment :name name
                      :variable (copy-list variable)
                      :symbol-macro (copy-list symbol-macro)
                      :function (copy-list function)
                      :macro (copy-list macro)
                      :declare (copy-list declare)
                      :next next
                      :declaration-handlers (and declaration-handlers
                                                 (alexandria:copy-hash-table
                                                   declaration-handlers)))))

;;;; DEFINE-DECLARATION

(defun list-all-declarations (&optional env)
  (loop :for decl-name :being :each :hash-key :of (declaration-handlers env)
        :collect decl-name))

(defun extend (type env contents)
  "Destructively extend the specified TYPE slot of the ENV with CONTENTS."
  (ecase type
    (:variable (alexandria:unionf (environment-variable env) contents))
    (:function (alexandria:unionf (environment-function env) contents)))
  env)

(defmacro define-declaration (decl-name lambda-list &body body)
  `(progn
    (setf (gethash ',decl-name (declaration-handlers *environment*))
            (lambda ,lambda-list ,@body))
    ',decl-name))

(defun pprint-define-declaration (out exp &rest noise)
  (declare (ignore noise))
  (funcall (formatter "~:<~W~^ ~1I~:_~W~^ ~:_~W~^ ~_~@{~W~^ ~_~}~:>") out exp))

(set-pprint-dispatch '(cons (member define-declaration))
                     'pprint-define-declaration)

(define-declaration special (form env)
  (destructuring-bind
      (decl-name &rest vars)
      form
    (extend :variable env vars)
    (values :variable (mapcar (lambda (var) (list var decl-name t)) vars))))

(define-condition bad-declaration-specifier (eprot-error type-error)
  ()
  (:report
   (lambda (this out)
     (format out "~S is not ~S." (type-error-datum this)
             (type-error-expected-type this)))))

(define-declaration type (form env)
  (declare (list form))
  (unless (< 2 (length form))
    (error 'bad-declaration-specifier
           :datum form
           :expected-type '(decl-name type &rest vars)))
  (destructuring-bind
      (decl-name type &rest vars)
      form
    (extend :variable env vars)
    (values :variable (mapcar (lambda (var) (list var decl-name type)) vars))))

(define-declaration ftype (form env)
  (declare (list form))
  (unless (< 2 (length form))
    (error 'bad-declaration-specifier
           :datum form
           :expected-type '(decl-name ftype &rest fun-names)))
  (destructuring-bind
      (decl-name ftype &rest names)
      form
    (extend :function env names)
    (values :function
            (mapcar (lambda (var) (list var decl-name ftype)) names))))

(define-declaration inline (form env)
  (destructuring-bind
      (decl-name &rest names)
      form
    (extend :function env names)
    (values :function (mapcar (lambda (var) (list var decl-name t)) names))))

(define-declaration notinline (form env)
  (destructuring-bind
      (decl-name &rest names)
      form
    (extend :function env names)
    (values :function (mapcar (lambda (var) (list var decl-name t)) names))))

(define-declaration optimize (form env)
  (declare (ignore env))
  (values :declare
          (cons (car form)
                (mapcar
                  (lambda (quality)
                    (if (symbolp quality)
                        (list quality 3)
                        quality))
                  (cdr form)))))

(define-declaration declaration (form env)
  (dolist (decl-name (cdr form))
    (setf (gethash decl-name (declaration-handlers env)) nil))
  (values :declare form))

(define-declaration dynamic-extent (form env)
  (extend :variable env (remove-if #'listp (cdr form)))
  (extend :function env
          (loop :for name :in (cdr form)
                :when (typep name '(cons (eql function) *))
                  :collect (cadr name)))
  (values :bind form))

(define-condition unknown-declaration (eprot-error cell-error)
  ()
  (:report
   (lambda (this out)
     (format out
             "~S is unknown as declaration for current null lexical environment ~S. ~:@_"
             (cell-error-name this)
             (environment-name (null-lexical-environment *environment*)))
     (did-you-mean out (cell-error-name this) (list-all-declarations))
     (format out
             " ~:@_Or it may for other environment. ~:@_Use ~S or bind ~S dinamically with ~S to change current environment."
             'in-environment '*environment* 'find-environment)
     (format out " ~:@_To see all environment, evaluate ~S."
             '(list-all-environments))
     (format out
             " ~:@_Or just not defined yet. ~:@_To see how to define declaration, evaluate ~S."
             `(describe 'define-declaration)))))

(defun default-decl-handler (decl-form env)
  (cond
    (;; Is declaration known one?
     (nth-value 1 (gethash (car decl-form) (declaration-handlers env)))
     nil)
    (;; Standard requires type-specifier is treated as type declaration.
     (millet:type-specifier-p (car decl-form))
     (values :variable
             (mapcar (lambda (var) (list var 'type (car decl-form)))
                     (cdr decl-form))))
    (t (error 'unknown-declaration :name (car decl-form)))))

(defun declaration-handler (decl-name &optional env)
  (if env
      ;; in order to accept { decl-name : nil } as just known declaration by proclaim.
      (or (gethash decl-name (declaration-handlers env)) 'default-decl-handler)
      'default-decl-handler))

(defun parse-declaration-spec (decl-spec &optional env)
  (let ((env (or env (null-lexical-environment))))
    (multiple-value-bind (type info)
        (funcall (coerce (declaration-handler (car decl-spec) env) 'function)
                 decl-spec env)
      (when type
        (make-decl-spec type info)))))

;;;; PROCLAIM

(defun proclaim (decl-spec)
  (let ((spec (parse-declaration-spec decl-spec *environment*)))
    (when spec
      (push spec (environment-declare *environment*)))))

;;;; IMPLEMENTATIONS
;;;; CONSTRUCTOR.
;;; AUGMENT-ENVIRONMENT.

(defun augment-environment
       (env &key variable symbol-macro function macro declare name)
  ;; Trivial type checks.
  #.(let ((assertions
           '((assert (every (lambda (elt) (typep elt 'var-name)) variable) ())
             (assert (every
                       (lambda (elt)
                         (typep elt '(cons var-name (cons t null))))
                       symbol-macro)
               ())
             (assert (every (lambda (elt) (typep elt 'function-name))
                            function))
             (assert (every
                       (lambda (elt)
                         (typep elt
                                '(cons function-name (cons function null))))
                       macro))
             (assert (every (lambda (elt) (typep elt '(cons symbol t)))
                            declare)))))
      (or #+(or sbcl lispworks cmucl ccl allegro) ; policy-cond supported.
          `(policy-cond:policy-cond
             ((< 0 safety) ,@assertions)
             (t nil)) ; The default.
          `(progn ,@assertions)))
  ;; CLTL2 require these error checks.
  (let ((intersect (intersection variable (mapcar #'car symbol-macro))))
    (when intersect
      (error 'simple-program-error
             :format-control "You could not specifay the same name for variable and symbol-macro. ~S"
             :format-arguments (list intersect))))
  (flet ((declared-special (spec)
           (loop :for (name . specs) :in spec
                 :if (eq 'special name)
                   :append specs)))
    (let ((intersect
           (intersection (mapcar #'car symbol-macro)
                         (declared-special declare))))
      (when intersect
        (error 'simple-program-error
               :format-control "You could not declare symbol-macro as special. ~S"
               :format-arguments (list intersect)))))
  (let ((intersect (intersection function (mapcar #'car macro))))
    (when intersect
      (error 'simple-program-error
             :format-control "You could not the same name for function and macro. ~S"
             :format-arguments (list intersect))))
  ;; CLTL2 recommends there error checks.
  #-sbcl
  (unless (typep env '(or null environment))
    (error 'type-error :datum env :expected-type '(or null environment)))
  ;; The body.
  (let ((*environment*
         (if env
             (make-environment :variable variable
                               :symbol-macro symbol-macro
                               :function function
                               :macro macro
                               :name name
                               :next env)
             (let ((new (copy-env)))
               (with-slots ((v variable) (sm symbol-macro) (f function)
                            (m macro) (n name))
                   new
                 (alexandria:appendf v variable)
                 (alexandria:appendf sm symbol-macro)
                 (alexandria:appendf f function)
                 (alexandria:appendf m macro)
                 (setf n name))
               new))))
    (mapc #'proclaim declare)
    *environment*))

;;;; ACCESSOR.
;;; VARIABLE-INFORMATION.

(defun related-declarations (name type &optional env)
  (uiop:while-collecting (acc)
    (do-env (e (or env (null-lexical-environment)))
      (dolist (spec (environment-declare e))
        (let ((info
               (and (or (eq type (decl-spec-type spec))
                        (eq :bind (decl-spec-type spec)))
                    (assoc name (decl-spec-info spec)
                           ;; NAME may (SETF fun-name).
                           :test #'equal))))
          (when info
            (acc (cons (second info) (third info)))))))))

(defun null-lexical-environment-p (env) (null (environment-next env)))

(defun variable-information (var-name &optional env)
  ;; CLTL2 recommends there error checks.
  #-sbcl
  (progn
   (check-type var-name var-name)
   (unless (typep env '(or null environment))
     (error 'type-error :datum env :expected-type '(or null environment))))
  (if (constantp var-name)
      (values :constant nil nil)
      (do-env (e (or env (null-lexical-environment)) #|FIXME|#
               (values nil nil nil))
        (when (find var-name (environment-variable e))
          (let ((decls (related-declarations var-name :variable e)))
            (return
             (values (cond ((assoc 'special decls) :special)
                           ((null-lexical-environment-p e) nil)
                           (t :lexical))
                     (not (null-lexical-environment-p e))
                     decls))))
        (when (assoc var-name (environment-symbol-macro e))
          (return
           (values :symbol-macro
                   (not (null-lexical-environment-p e))
                   (related-declarations var-name :variable e)))))))

;;; FUNCTION-INFORMATION.

(defun function-information (fun-name &optional env)
  ;; CLTL2 recommends there error checks.
  #-sbcl
  (progn
   (check-type fun-name function-name)
   (unless (typep env '(or null environment))
     (error 'type-error :datum env :expected-type '(or null environment))))
  (do-env (e (or env (null-lexical-environment)) #|FIXME|# (values nil nil nil))
    (when (find fun-name (environment-function e))
      (return
       (values :function
               (not (null-lexical-environment-p e))
               (related-declarations fun-name :function e))))
    (when (assoc fun-name (environment-macro e))
      (return
       (values :macro
               (not (null-lexical-environment-p e))
               (related-declarations fun-name :function e))))))

;;; DECLARATION-INFORMATION

(defun declaration-information (decl-name &optional env)
  ;; CLTL2 recommends there error checks.
  #-sbcl
  (progn
   (check-type decl-name declaration-name)
   (unless (typep env '(or null environment))
     (error 'type-error :datum env :expected-type '(or null environment))))
  (uiop:while-collecting (acc)
    (do-env (e (or env (null-lexical-environment)))
      (dolist (spec (environment-declare e))
        (if (eq decl-name (decl-spec-type spec))
            (acc (decl-spec-info spec)))))))

;;;; PARSE-MACRO

(defun parse-macro (name lambda-list body &optional env)
  (declare (ignore env))
  #+(or clisp allegro abcl)
  (progn (check-type name macro-name) (check-type body list))
  #+(or clisp cmucl allegro abcl)
  (check-type env (or null environment))
  (let ((?form
         (or (lambda-fiddle:whole-lambda-var lambda-list) (gensym "WHOLE")))
        (?env
         (or (lambda-fiddle:environment-lambda-var lambda-list)
             (gensym "ENVIRONMENT"))))
    `(lambda (,?form ,?env)
       ,@(unless (symbol-package ?env)
           `((declare (ignore ,?env))))
       (destructuring-bind
           ,(lambda-fiddle:remove-environment-part
              (lambda-fiddle:remove-whole-part lambda-list))
           (cdr ,?form)
         (block ,name ,@body)))))

;;;; ENCLOSE

(defun enclose (lambda-expression &optional env)
  (declare (ignore env))
  #+(or clisp cmucl allegro abcl)
  (progn (check-type env (or null environment)))
  (coerce lambda-expression 'function))

;;;; MACRO-FUNCTION

(defun macro-function (symbol &optional env)
  #+(or clisp allegro abcl)
  (progn (check-type symbol symbol))
  (do-env (e (or env (null-lexical-environment)))
    (let ((definition (assoc symbol (environment-macro e))))
      (return (cadr definition)))))

;;;; *MACROEXPAND-HOOK*

(defvar *macroexpand-hook* 'funcall)

;;;; MACROEXPAND-1

(defun macroexpand-1 (form &optional env)
  (let ((env (or env (null-lexical-environment))))
    (if (symbolp form)
        (let ((exists?
               (do-env (e env)
                 (let ((definition (assoc form (environment-symbol-macro e))))
                   (when definition
                     (return
                      (funcall (coerce *macroexpand-hook* 'function)
                               (constantly (cadr definition)) form env)))))))
          (if exists?
              (values exists? t)
              (values form nil)))
        (if (atom form)
            (values form nil)
            (let ((expander (macro-function (car form) env)))
              (if expander
                  (values (funcall (coerce *macroexpand-hook* 'function)
                                   expander form env)
                          t)
                  (values form nil)))))))

;;;; MACROEXPAND

(defun macroexpand (form &optional env)
  (multiple-value-bind (form expanded?)
      (macroexpand-1 form env)
    (if expanded?
        (macroexpand form env)
        form)))