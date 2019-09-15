(in-package :transexpr)

(setf (readtable-case *readtable*) :invert)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Level 0: Purely Syntactic ;;
;;     Gives you macros      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *transexpr-log* nil)

(defparameter *infix-ops* (mapcar #'symbol-name '(+ - * / % = == !=)))

(defmacro defform (name params &rest body)
  `(defmacro ,(intern (symbol-name name) 'ts-symspace)
       ,params
     ,@body))

(defgeneric transpile (ast)
  (:documentation "list to code"))

(defgeneric transpile-funcall (operator expr)
  (:documentation "redefine this function to expand function call"))

(defmethod transpile :around (ast)
  (let ((expr (call-next-method)))
    (if (listp expr)
	(apply #'format (cons nil expr))
	expr)))

(defun joinstrs (separator xs &optional (acc ""))
  (cond ((null xs) "")
	((null (cdr xs))
	 (concatenate 'string acc (string (car xs))))
	(t (joinstrs separator (cdr xs)
		     (concatenate 'string acc (string (car xs)) separator)))))

(defun paren (open close expr)
  (assert (stringp expr))
  (concatenate 'string open expr close))

(defun symbols-which (package pred)
  "Find symbols residing in the package."
  (let ((pkg (find-package package))
	(xs ()))
    (do-symbols (sym (find-package pkg))
      (if (and (eq pkg (symbol-package sym))
	       (funcall pred sym))
	  (push sym xs))) xs))

(defmethod transpile ((sym symbol))
  (intern (symbol-name sym) 'ts-symspace))

(defmethod transpile ((expr list))
  (multiple-value-bind (form _)
      (let ((operator (transpile (car expr))))
	(if *transexpr-log* (format t "LOG: ~A ~%" operator))
	(cond ((not (symbolp operator))
	       (warn "Not implemented"))
	      ((macro-function operator)
	       (macroexpand (cons operator (cdr expr))))
	      (t (transpile-funcall operator expr))))
    (declare (ignore _))
    form))

