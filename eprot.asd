; vim: ft=lisp et
(in-package :asdf)
(defsystem "eprot"
  :version
  "0.22.0"
  :depends-on
  (
   "alexandria"         ; Public domain utilities.
   "millet"             ; Wrapper about implementation dependent tiny utilities.
   "policy-cond"        ; Control flow macro that interprets declaration information.
   "lambda-fiddle"      ; Lambda list handling.
   "fuzzy-match"        ; For better error message.
   )
  :pathname
  "src/"
  :components
  ((:file "eprot"))
  :author "SATO Shinichi"
  :license "MIT"
  :description "General purpose CLTL2 compatible environment protocol.")

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "eprot").
(defmethod component-depends-on ((o test-op) (c (eql (find-system "eprot"))))
  (append (call-next-method) '((test-op "eprot.test"))))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
(defmethod operate :around
           ((o test-op) (c (eql (find-system "eprot")))
            &rest keys
            &key ((:compile-print *compile-print*))
            ((:compile-verbose *compile-verbose*)) &allow-other-keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
;; Enable importing spec documentations.
(let ((system (find-system "jingoh.documentizer" nil)))
  (when system
    (load-system system)
    (defmethod perform :after ((o load-op) (c (eql (find-system "eprot"))))
      (with-muffled-conditions (*uninteresting-conditions*)
        (handler-case (symbol-call :jingoh.documentizer :import c)
                      (error (condition)
                             (warn "Fails to import documentation of ~S.~%~A"
                                   (coerce-name c)
                                   (princ-to-string condition))))))))
