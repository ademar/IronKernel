
; IronKernel samples

(define fact 
	(lambda (n) 
		(if (<= n 1) 
			1 
			(* n (fact (- n 1))))))


; .NET interop
(define write 
	(lambda (x) 
	(. System.Console Write x)))



; Continuations

(define frozen #f)

(+ 2 (call/cc (lambda (k) (begin (set! frozen k) 3))))
