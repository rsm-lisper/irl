;; -*- mode: lisp -*-

(defpackage :interpr
  (:use :common-lisp)
  (:documentation "interpreter rachunku lambda"))
(in-package :interpr)

(defconstant +repl-prompt+ "λ> ")
(defconstant +welcome-message+
  "interpreter rachunku lambda. autor rsm. na licencji mit. (wyjście: C-c)")


(defun eval-exp (s-exp &optional (env (lambda (y) y)))
  "ewaluator rachunku lambda."
  (cond ((symbolp s-exp) (funcall env s-exp))
	((and (listp s-exp) (eq 'lambda (car s-exp)))
	 (lambda (arg)
	   (eval-exp (caddr s-exp) (lambda (y)
				     (if (eq y (caadr s-exp))
					 arg
					 (funcall env y))))))
	((and (listp s-exp) (= 2 (length s-exp)))
	 (funcall (eval-exp (car s-exp) env)
		  (eval-exp (cadr s-exp) env)))
	(t '*eval-error*)))


(defun print-exp (x)
  "printer - zamienia wynik na wersję tekstową."
  (format t "~&~A~%~A" x +repl-prompt+)
  (finish-output))


(defun repl-exp ()
  "główna pętla (tzw repl - read eval print loop) interpretera."
  (print-exp +welcome-message+)
  (loop (print-exp (eval-exp (read)))))

(repl-exp)
