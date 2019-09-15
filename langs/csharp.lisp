(defpackage :transexpr-csharp
  (:use :cl :transexpr))

(in-package :transexpr-csharp)

;;compiler state
(defparameter *csharp-classes* ())
(defparameter *current-csharp-class* ())
(defparameter *global-delegates* ())

;;symbols
(setf *infix-ops* (mapcar #'symbol-name '(+ - * / % = == != < > <= >=)))
(defparameter ts-symspace::true "true")
(defparameter ts-symspace::false "false")
(defparameter ts-symspace::null "null")


;;todo hiphen to camel-case
(defun sym->tok (sym)
  (let ((sname (symbol-name sym)))
    (if (loop :for x across sname
	   :always (upper-case-p x))
	(string-downcase sname)
	sname)))

(defun purify (expr)
  (cond ((symbolp expr)
	 (sym->tok expr))
	((stringp expr)
	 expr)
	(t (warn "inpure "))))

(defun str (&rest strs)
  (apply #'concatenate (cons 'string strs)))

(defun pair->typed-tok (spec)
  (joinstrs " " (mapcar #'sym->tok spec)))

(defun params (params)
  (paren "(" ")" (joinstrs "," (mapcar #'pair->typed-tok params))))

(defun insert-return (body-strs &optional (name-spec ()))
    (concatenate
     'list (butlast body-strs)
     (list (str
	    (if (eq 'void (car (last name-spec))) "" "return ")
	    (car (last body-strs)) ";"))))

(defmethod transpile-funcall (operator expr)
  (let ((operands (mapcar #'purify (mapcar #'transpile (cdr expr)))))
    (if (member (symbol-name operator) *infix-ops*
		:test #'string=)
	(paren "(" ")" (joinstrs (sym->tok operator) operands))
	(str (sym->tok operator)
	     (paren "(" ")" (joinstrs ", " operands))))))


(defmethod transpile ((num number))
  (format nil "~A" (float num)))

(defmethod transpile ((str string))
  (format nil "'~A'" str))

(defform if (pred then else)
  (list "if (~A) { ~A } else { ~A }"
	(transpile pred)
	(transpile then)
	(transpile else)))

(defform def (type name &optional init-val)
  (list "~%~tpublic ~A ~A ~A"
	(sym->tok type)
	(sym->tok name)
	(if init-val (str "= " (transpile init-val) ";")
	    ";")))

(defform defun (name-spec params &rest body)
  (let* ((body-strs (mapcar #'transpile body))
	 (body-strs (insert-return body-strs name-spec)))
    (list "~%~t~A ~A { ~A }"
	  (pair->typed-tok (list 'public (car (last name-spec))
				 (car name-spec)))
	  (params (mapcar #'list (butlast (cdr name-spec)) params))
	  (joinstrs "; " body-strs))))

(defform lam (params &rest exprs)
  (list "(~A) => { ~A }"
	(joinstrs ", " (mapcar #'sym->tok params))
	(joinstrs "; " (insert-return (mapcar #'transpile exprs)))))

(defstruct csharp-class
  name parents slots methods)

(defun find-csharp-class (name)
  (find-if (lambda (x) (eq (csharp-class-name x) name))
	   *csharp-classes*))

(defform defclass (name parents slots)
  (push (make-csharp-class
	 :name name
	 :parents parents
	 :slots slots
	 :methods ())
	*csharp-classes*)
  "")

(defform defmethod (name-spec class-name params &rest body)
  (let ((class (find-if (lambda (x) (eq (csharp-class-name x) class-name))
			*csharp-classes*)))
    (setf *current-csharp-class* class)
    (push
     `(ts-symspace::defun ,name-spec ,params ,@body)
     (csharp-class-methods
      class)))
  "")

(defform class (name parents slots methods)
  (list "~%class ~A ~A {~% ~A ~A ~%}"
	(sym->tok name)
	(if parents (str " : "(joinstrs ", " parents)) "")
	(joinstrs " " (mapcar #'transpile slots))
	(joinstrs " " (let ((specs ()))
			(mapcar (lambda (method)
				  (if (member (cadr method) specs :test 'equal)
				      ""
				      (progn
					(push (cadr method) specs)
					(transpile method)))) methods)))))

(defform compile-class (class-name)
  (let ((class (find-csharp-class class-name)))
    (setf *current-csharp-class* class)
    (with-slots (name parents slots methods)
	class
      `(ts-symspace::class ,name ,parents ,slots ,methods))))

(defun clear-methods (class-name)
  (setf (csharp-class-methods (find-csharp-class class-name)) nil))


(defform defdelegate (privacy type name params)
  (list "~A delegate ~A ~A ~A;"
	(sym->tok privacy)
	(sym->tok type)
	(sym->tok name)
	(params params)))

(defform delegate (params &rest body)
  (list "(delegate ~A {~%~t ~A ~%~t})"
	(params params)
	(joinstrs "; " (insert-return (mapcar #'transpile body)))))

(defform lambda (name-spec params &rest body)
  (let ((class *current-csharp-class*))
    (unless (find-if (lambda (x) (eq (cadddr x) (car name-spec)))
		     *global-delegates*)
      (push
       `(defdelegate public
	    ,(car (last name-spec))
	  ,(car name-spec)
	  ,(mapcar 'list (butlast (cdr name-spec)) params))
      *global-delegates*))
    `(ts-symspace::delegate ,(mapcar 'list (butlast (cdr name-spec)) params)
			    ,@body)))

(defparameter *source*
  '((defclass foo ()
      ((def List<float> xs)))

    (defmethod (ho TestName bool) foo (f)
	       (xs.All (lam (x) (f x))))

    (defmethod (isEmpty void bool) foo ()
	       (ho (lambda (TestName float bool) (x) (< x 1))))
    
    (compile-class foo)
    ))

;; example
(progn
  (setf *csharp-classes* nil)
  (setf *global-delegates* nil)
  (let* ((src (mapcar #'transpile *source*))
	 (dgs (mapcar #'transpile *global-delegates*)))
    (loop for x in (concatenate 'list dgs src)
       do (when (> (length x) 0)
	    (terpri)
	    (format t x)))))
