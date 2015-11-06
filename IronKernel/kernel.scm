
; IronKernel bootstrapping library
;

(define quote (vau (x) _ x))

(define sequence
  ((wrap
     (vau (seq2) _
          (seq2
            (define aux
              (vau (head & tail) env
                   (if (null? tail)
                     (eval env head)
                     (seq2
                       (eval env head)
                       (eval env (cons aux tail))))))
            (vau body env
                 (if (null? body)
                   #inert
                   (eval env (cons aux body)))))))
   (vau (first second) env
        ((wrap (vau _ _ (eval env second)))
         (eval env first)))))

(define list (wrap (vau x _ x)))

(define list*
  (wrap
    (vau args _
         (sequence
           (define aux
             (wrap
               (vau ((head & tail)) _
                    (if (null? tail)
                      head
                      (cons head (aux tail))))))
           (aux args)))))

;TODO: consider making length a native primitive		   
(define length
  (wrap
    (vau (x) _
      (if (null? x)
        0
        (+ 1 (length (cdr x)))))))		   
		   
; This operative generalizes primitive operative $vau , §4.10.3, so that the con-
;structed compound operative will evaluate a sequence of expressions in its local en-
;vironment, rather than just one.

(define vau
  ((wrap
     (vau (vau) _
          (vau (formals eformal & body) env
               (eval env (list vau formals eformal
                           (if (> (length body) 1)
                             (cons sequence body)
                             (car body)))))))
   vau))		   
			
; lambda is built in terms of vau
(define lambda 
	(vau (params & body) static-env 
		; inlined comment
		(wrap (eval static-env (list* vau params '_ body)))))

(define λ lambda)
		
(define apply 
	(lambda (appv arg & opt)
		(eval 
			(if (null? opt)
				(make-environment)
				(car opt))
			(cons (unwrap appv) arg))))

(define last (lambda (xs)
    (if (null? (cdr xs))
        (car xs)
        (last (cdr xs)))))

(define begin (lambda xs (last xs)))

(define map
  (lambda (f xs)
    (if (null? xs)
      ()
      (cons (f (car xs)) (map f (cdr xs))))))
	  
(define caar (lambda (((x & _) & _)) x))
(define cdar (lambda (((_ & x) & _)) x))
(define cadr (lambda ((_ & (x & _))) x))
(define cddr (lambda ((_ & (_ & x))) x))	  

(define let (vau (binds & body) env
    (eval env
        (cons
            (list* lambda (cons (map car binds) body))
            (map cadr binds)))))

(define zip
  (lambda (f & xss)
    (let ((rest (map cdr xss)))
      (cons (apply f (map car xss))
            (if (any? null? rest)
              ()
              (apply zip (cons f rest)))))))

(define let*
  (vau (bindings & body) env
    (eval env (if (null? bindings)
            (list* let bindings body)
            (list let (list (car bindings))
                  (list* let* (cdr bindings) body))))))

(define letrec
  (vau (bindings & body) env
    (eval env (list* let ()
                 (list define
                   (map car bindings)
                   (cons list (map cadr bindings)))
                 body))))

(define letrec*
  (vau (bindings & body) env
    (eval env (if (null? bindings)
            (list* letrec bindings body)
            (list letrec (list (car bindings))
                  (list* letrec* (cdr bindings) body))))))

(define let-redirect
  (vau (exp bindings & body) env
    (eval env (cons (eval (eval env exp) (list* lambda (map car bindings) body))
                (map cadr bindings)))))

(define for-each
  (wrap (vau (x f) env (apply map (list f x) env) #inert)))
  
(define get-current-environment (wrap (vau () e e)))

(define remote-eval
  (vau (o e) d
    (eval (eval d e) o)))

(define bindings->environment
  (vau bindings env
    (eval env (list let-redirect
                (make-environment)
                bindings
                (list get-current-environment)))))
		  
(define provide!
  (vau (symbols & body) env
    (eval env (list define symbols
                (list let ()
                      (cons sequence body)
                      (cons list symbols))))))
					  
(define set!
  (vau (target formals values) env
    (eval (eval env target) (list define formals (list (unwrap eval) env values)))))					  

(define import!
  (vau (exp & symbols) env
    (eval (eval env exp) (list set! env symbols (cons list symbols)))))		  

(define defn
  (vau (name & body) e
    (if (pair? name)
      (let (((n & as) name))
        (eval e (list define n
                    (cons lambda (cons as body)))))
      (eval e (cons define (cons name body))))))

(defn (compose f g) (lambda (x) (f (g x))))

(define let/cc
	(vau (symbol & body) env
		(eval env (list call/cc (list* lambda (list symbol) body)))))

; the time operative benchmarks expression evaluation
(define time (vau (x) env 
	(let* 
		((start (.get System.DateTime Now)) 
		 (result (eval env x))) 
		 (begin 
			(printf "Time elapsed: {0} milliseconds\n" (.get (- (.get System.DateTime Now) start) TotalMilliseconds)) result))))
			
(define cond 
	(vau clauses env
		(define aux
			(lambda ((test & body) & clauses)
				(if (eval env test)
					(apply (wrap sequence) body env)
					(apply (wrap cond) clauses env))))
					
		(if (null? clauses) #inert (apply aux clauses))))			

(define not? (lambda (x) (if x #f #t)))

(define and?
   (vau x e
      (cond ((null? x)         #t)
             ((null? (cdr x))   (eval e (car x)))
             ((eval e (car x))  (apply (wrap and?) (cdr x) e))
             (#t                #f))))
			 
(define or?
	(vau x e
		(cond ((null? x)	#f)
			((null? (cdr x)) (eval e (car x)))
			((eval e (car x)) #t)
			(#t			(apply (wrap or?) (cdr x) e)))))