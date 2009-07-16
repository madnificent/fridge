(defpackage :fridge.sysdef
  (:use :asdf :common-lisp))
(in-package :fridge.sysdef)

(defsystem :fridge
    :name "Fridge"
    :author "Aad Versteden <madnificent@gmail.com>"
    :version "0.1"
    :maintainer "Aad Versteden <madnificent@gmail.com>"
    :licence "MIT"
    :description "Fridge is a connection from lisp to the database."
    :depends-on  (:closer-mop :postmodern :versioned-objects :fiveam :database-migrations :cl-ppcre :s-sql :validations)
    :components ((:file "fridge")
		 (:file "fridge-stored" :depends-on ("fridge"))
		 (:file "fridge-linked" :depends-on ("fridge-stored"))
		 (:file "fridge-inferred" :depends-on ("fridge-linked"))
		 (:file "fridge-tests" :depends-on ("fridge" "fridge-stored" "fridge-linked"))))
