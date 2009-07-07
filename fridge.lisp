(defpackage :fridge
  (:use :common-lisp :postmodern)
  (:export :load-instance
	   :save))

(defpackage :fridge-user
  (:use :common-lisp :fridge))

(in-package :fridge)

