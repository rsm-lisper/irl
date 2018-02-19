;; -*- mode: scheme -*-


(define +repl-prompt+ "λ> ")
(define +welcome-message+
  "interpreter rachunku lambda. autor rsm. na licencji mit. (wyjście: C-c)")


(define (eval-exp s-exp env)
  "ewaluator rachunku lambda."
  (cond ((symbol? s-exp) (env s-exp))
	((and (list? s-exp) (eq? 'lambda (car s-exp)))
	 (lambda (arg)
	   (eval-exp (caddr s-exp)
		     (lambda (y)
		       (if (eq? y (caadr s-exp))
			   arg
			   (env y))))))
	((and (list? s-exp) (= 2 (length s-exp)))
	 ((eval-exp (car s-exp) env) (eval-exp (cadr s-exp) env)))
	(else '*eval-error*)))


(define (print-exp x)
  "printer - zamienia wynik na wersję tekstową."
  (display x) (newline)
  (display +repl-prompt+))


(define (repl)
  "główna pętla (tzw repl - read eval print loop) interpretera."
  (print-exp (eval-exp (read)
		       (lambda (e) e)))
  (repl))


(print-exp +welcome-message+)
(repl)
