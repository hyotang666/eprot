(in-package :cl-user)

(defpackage :eprot
  (:use :cl)
  (:shadow . #0=(macro-function macroexpand-1 macroexpand *macroexpand-hook*))
  (:export ; cl things.
           .
           #0#)
  (:export ; cltl2 things.
           #:augment-environment
           #:variable-information
           #:function-information
           #:declaration-information
           #:parse-macro
           #:enclose)
  (:export ; EPROT things.
           #:environment
           #:*environment*))

(in-package :eprot)

;;;; TODO
#|
* defmacro
* define-declaration
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
* proclaim
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

;;;; ENVIRONMENT OBJECT

(defstruct environment
  (variable nil :type list :read-only t)
  (symbol-macro nil :type list :read-only t)
  (function nil :type list :read-only t)
  (macro nil :type list :read-only t)
  (declare nil :type list :read-only t)
  (next nil :type (or null environment) :read-only t))

(defmethod print-object ((o environment) output)
  (cond (*print-readably* (call-next-method))
        (t (print-unreadable-object (o output :type t :identity t)))))

;;;; ITERATOR

(defmacro do-env ((var <environment> &optional <return>) &body body)
  `(do ((,var ,<environment> (environment-next ,var)))
       ((null ,var) ,<return>)
    ,@body))

;;;; SPECIAL VARIABLE

(defvar *environment*)

(declaim (environment *environment*))

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

;;;; FTYPES

(declaim
 (ftype (function (var-name &optional (or null environment))
         (values (or null variable-type) lexicalp declaration-alist &optional))
        variable-information)
 (ftype (function (function-name &optional (or null environment))
         (values (or null function-type) lexicalp declaration-alist &optional))
        function-information)
 (ftype (function (declaration-name &optional (or null environment))
         (values t &optional))
        declaration-information)
 (ftype (function
         ((or null environment) &key (:variable list) (:symbol-macro list)
          (:function list) (:macro list) (:declare list))
         (values environment &optional))
        augment-environment)
 (ftype (function (macro-name lambda-list body &optional (or null environment))
         (values lambda-expression &optional))
        parse-macro)
 (ftype (function (lambda-expression &optional (or null environment))
         (values function &optional))
        enclose))

;;;; IMPLEMENTATIONS
;;;; CONSTRUCTOR.
;;; AUGMENT-ENVIRONMENT.

(defun augment-environment
       (env &key variable symbol-macro function macro declare)
  ;; Trivial type checks.
  (policy-cond:policy-cond
    ((< 0 safety)
     (assert (every (lambda (elt) (typep elt 'var-name)) variable) ())
     (assert (every (lambda (elt) (typep elt '(cons var-name (cons t null))))
                    symbol-macro)
       ())
     (assert (every (lambda (elt) (typep elt 'function-name)) function))
     (assert (every
               (lambda (elt)
                 (typep elt '(cons function-name (cons function null))))
               macro))
     (assert (every (lambda (elt) (typep elt '(cons symbol t))) declare)))
    (t nil))
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
  (make-environment :variable variable
                    :symbol-macro symbol-macro
                    :function function
                    :macro macro
                    :declare declare
                    :next env))

;;;; ACCESSOR.
;;; VARIABLE-INFORMATION.

(defun variable-information (var-name &optional env)
  ;; CLTL2 recommends there error checks.
  #-sbcl
  (unless (typep env '(or null environment))
    (error 'type-error :datum env :expected-type '(or null environment)))
  (flet ((declared-specialp (declare)
           (and (eq 'special (car declare)) (find var-name (cdr declare)))))
    (if (constantp var-name)
        (values :constant t nil)
        (do-env (e env #|FIXME|# (values nil nil nil))
          (when (find var-name (environment-variable e))
            (return
             (values (if (find-if #'declared-specialp (environment-declare e))
                         :special
                         :lexical)
                     t
                     (environment-declare e))))
          (when (assoc var-name (environment-symbol-macro e))
            (return (values :symbol-macro t (environment-declare e))))))))

;;; FUNCTION-INFORMATION.

(defun function-information (fun-name &optional env)
  ;; CLTL2 recommends there error checks.
  #-sbcl
  (unless (typep env '(or null environment))
    (error 'type-error :datum env :expected-type '(or null environment)))
  (do-env (e env #|FIXME|# (values nil nil nil))
    (when (find fun-name (environment-function e))
      (return (values :function t (environment-declare e))))
    (when (find fun-name (environment-macro e))
      (return (values :macro t (environment-declare e))))))

;;; DECLARATION-INFORMATION

(defun declaration-information (decl-name &optional env)
  ;; CLTL2 recommends there error checks.
  #-sbcl
  (unless (typep env '(or null environment))
    (error 'type-error :datum env :expected-type '(or null environment)))
  (do-env (e env)
    (let ((declare
           (remove-if-not decl-name (environment-declare e) :key #'car)))
      (when declare
        (return
         (loop :for info :in declare
               :collect (cdr info)))))))

;;;; PARSE-MACRO

(defun parse-macro (name lambda-list body &optional env)
  (declare (ignore env))
  (let ((?form
         (or (lambda-fiddle:whole-lambda-var lambda-list) (gensym "WHOLE")))
        (?env
         (or (lambda-fiddle:environment-lambda-var lambda-list)
             (gensym "ENVIRONMENT"))))
    `(lambda (,?form ,?env)
       (destructuring-bind
           ,(lambda-fiddle:remove-environment-part
              (lambda-fiddle:remove-whole-part lambda-list))
           (cdr ,?form)
         (block ,name ,@body)))))

;;;; ENCLOSE

(defun enclose (lambda-expression &optional env)
  (declare (ignore env))
  (coerce lambda-expression 'function))

;;;; MACRO-FUNCTION

(defun macro-function (symbol &optional environment)
  (do-env (e environment)
    (let ((definition (assoc symbol (environment-macro e))))
      (return (cadr definition)))))

;;;; *MACROEXPAND-HOOK*

(defvar *macroexpand-hook* 'funcall)

;;;; MACROEXPAND-1

(defun macroexpand-1 (form &optional environment)
  (if (symbolp form)
      (let ((exists?
             (do-env (e environment)
               (let ((definition (assoc form (environment-symbol-macro e))))
                 (when definition
                   (return
                    (funcall *macroexpand-hook* (constantly (cadr definition))
                             form environment)))))))
        (if exists?
            (values exists? t)
            (values form nil)))
      (if (atom form)
          (values form nil)
          (let ((expander (macro-function (car form) environment)))
            (if expander
                (values (funcall *macroexpand-hook* expander form environment)
                        t)
                (values form nil))))))

;;;; MACROEXPAND

(defun macroexpand (form &optional environment)
  (multiple-value-bind (form expanded?)
      (macroexpand-1 form environment)
    (if expanded?
        (macroexpand form environment)
        form)))