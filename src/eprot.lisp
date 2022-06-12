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
           #:enclose
           #:define-declaration)
  (:export ; EPROT things.
           #:environment
           #:*environment*))

(in-package :eprot)

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

(defun did-you-mean (out name sellection)
  (apply (formatter "Did you mean ~#[~;~S~;~S or ~S~:;~S, ~S, or ~S~] ?") out
         (fuzzy-match:fuzzy-match (symbol-name name) sellection)))

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

;;;; DEFINE-DECLARATION

(defvar *declaration-handlers* (make-hash-table))

(defun list-all-declarations ()
  (loop :for decl-name :being :each :hash-key :of *declaration-handlers*
        :collect decl-name))

(defmacro define-declaration (decl-name lambda-list &body body)
  `(progn
    (setf (gethash ',decl-name *declaration-handlers*)
            (lambda ,lambda-list ,@body))
    ',decl-name))

(defun pprint-define-declaration (out exp &rest noise)
  (declare (ignore noise))
  (funcall (formatter "~:<~W~^ ~1I~:_~W~^ ~:_~W~^ ~_~@{~W~^ ~_~}~:>") out exp))

(set-pprint-dispatch '(cons (member define-declaration))
                     'pprint-define-declaration)

(define-declaration special (form env)
  (declare (ignore env))
  (values :variable (mapcar (lambda (var) (list var (car form) t)) (cdr form))))

(define-declaration type (form env)
  (declare (ignore env))
  (values :variable
          (mapcar (lambda (var) (list var (car form) (cadr form)))
                  (cddr form))))

(define-declaration ftype (form env)
  (declare (ignore env))
  (values :function
          (mapcar (lambda (var) (list var (car form) (cadr form)))
                  (cddr form))))

(define-declaration inline (form env)
  (declare (ignore env))
  (values :function (mapcar (lambda (var) (list var (car form) t)) (cdr form))))

(define-declaration notinline (form env)
  (declare (ignore env))
  (values :function (mapcar (lambda (var) (list var (car form) t)) (cdr form))))

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
  (declare (ignore env))
  (values :declare form))

(define-declaration dynamic-extent (form env)
  (declare (ignore env))
  (values :bind form))

(define-condition unknown-declaration (eprot-error cell-error)
  ()
  (:report
   (lambda (this out)
     (format out "~S is unknown as declaration. ~:@_" (cell-error-name this))
     (did-you-mean out (cell-error-name this) (list-all-declarations))
     (format out
             " ~:@_Or just not defined yet. ~:@_To see how to define declaration, evaluate ~S."
             `(describe 'define-declaration)))))

(defun default-decl-handler (decl-form env)
  (declare (ignore env))
  (if (millet:type-specifier-p (car decl-form))
      (values :variable
              (mapcar (lambda (var) (list var 'type (car decl-form)))
                      (cdr decl-form)))
      (error 'unknown-declaration :name (car decl-form))))

(defun declaration-handler (decl-name)
  (gethash decl-name *declaration-handlers* 'default-decl-handler))

;;;; DECL-SPEC

(defstruct (decl-spec (:constructor make-decl-spec (type info)))
  (type (error "TYPE is required.")
        :type (member :variable :function :declare :bind)
        :read-only t)
  (info nil :type list :read-only t))

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
        enclose)
 (ftype (function (symbol &optional (or null environment))
         (values (or null function) &optional))
        macro-function)
 (ftype (function (t &optional (or null environment))
         (values t boolean &optional))
        macroexpand-1))

;;;; IMPLEMENTATIONS
;;;; CONSTRUCTOR.
;;; AUGMENT-ENVIRONMENT.

(defun augment-environment
       (env &key variable symbol-macro function macro declare)
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
  (make-environment :variable variable
                    :symbol-macro symbol-macro
                    :function function
                    :macro macro
                    :declare (mapcar
                               (lambda (decl-spec)
                                 (multiple-value-call #'make-decl-spec
                                   (funcall
                                     (declaration-handler (car decl-spec))
                                     decl-spec env)))
                               declare)
                    :next env))

;;;; ACCESSOR.
;;; VARIABLE-INFORMATION.

(defun related-declarations (name type env)
  (uiop:while-collecting (acc)
    (do-env (e env)
      (dolist (spec (environment-declare e))
        (let ((info
               (and (or (eq type (decl-spec-type spec))
                        (eq :bind (decl-spec-type spec)))
                    (assoc name (decl-spec-info spec)
                           ;; NAME may (SETF fun-name).
                           :test #'equal))))
          (when info
            (acc (cons (second info) (third info)))))))))

(defun variable-information (var-name &optional env)
  ;; CLTL2 recommends there error checks.
  #-sbcl
  (progn
   (check-type var-name var-name)
   (unless (typep env '(or null environment))
     (error 'type-error :datum env :expected-type '(or null environment))))
  (if (constantp var-name)
      (values :constant t nil)
      (do-env (e env #|FIXME|# (values nil nil nil))
        (when (find var-name (environment-variable e))
          (let ((decls (related-declarations var-name :variable e)))
            (return
             (values (if (assoc 'special decls)
                         :special
                         :lexical)
                     t
                     decls))))
        (when (assoc var-name (environment-symbol-macro e))
          (return
           (values :symbol-macro
                   t
                   (related-declarations var-name :variable e)))))))

;;; FUNCTION-INFORMATION.

(defun function-information (fun-name &optional env)
  ;; CLTL2 recommends there error checks.
  #-sbcl
  (progn
   (check-type fun-name function-name)
   (unless (typep env '(or null environment))
     (error 'type-error :datum env :expected-type '(or null environment))))
  (do-env (e env #|FIXME|# (values nil nil nil))
    (when (find fun-name (environment-function e))
      (return (values :function t (related-declarations fun-name :function e))))
    (when (assoc fun-name (environment-macro e))
      (return (values :macro t (related-declarations fun-name :function e))))))

;;; DECLARATION-INFORMATION

(defun declaration-information (decl-name &optional env)
  ;; CLTL2 recommends there error checks.
  #-sbcl
  (progn
   (check-type decl-name declaration-name)
   (unless (typep env '(or null environment))
     (error 'type-error :datum env :expected-type '(or null environment))))
  (uiop:while-collecting (acc)
    (do-env (e env)
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

(defun macro-function (symbol &optional environment)
  #+(or clisp allegro abcl)
  (progn (check-type symbol symbol))
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