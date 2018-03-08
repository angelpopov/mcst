;;;; mcst.asd

(asdf:defsystem #:mcst
  :description "Monte Carlo tree search"
  :author "Angel Popov"
  :depends-on (#:alexandria)
  :serial t
  :components ((:file "package")
               (:file "mcst")))

