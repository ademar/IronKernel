
; IronKernel bootstrapping library
;

(define quote (vau (x) _ x))

(define list 
	(vau xs env 
		(if (null? xs) 
		'() 
		(cons 
			(eval env (car xs)) 
			(eval env (cons list (cdr xs)))))))


; lambda is build on terms of vau
(define lambda 
	(vau (params body) static-env 
		; inlined comment
		(wrap (eval static-env (list vau params '_ body)))))


(define last (lambda (xs)
    (if (null? (cdr xs))
        (car xs)
        (last (cdr xs)))))

(define begin (lambda xs (last xs)))

; lets re-write lambda to take a body with multiple elements

(set! lambda
    ((lambda (base-lambda)
        (vau (param body) env
            (eval env (list base-lambda param (cons begin body)))))
    lambda))

; Closing comment