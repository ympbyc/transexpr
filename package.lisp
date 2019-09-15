(defpackage :ts-symspace)

(defpackage :transexpr
  (:use cl)
  (:export :transpile :defform :joinstrs :paren :*infix-ops*))
