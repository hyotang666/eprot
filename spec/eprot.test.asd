; vim: ft=lisp et
(in-package :asdf)
(defsystem "eprot.test"
  :version
  "0.16.0"
  :depends-on
  (:jingoh "eprot")
  :components
  ((:file "eprot"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :eprot args)))
