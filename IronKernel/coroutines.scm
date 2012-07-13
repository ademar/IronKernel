
(define display 
	(lambda (x) 
	(. System.Console Write x)))

(define newline 
	(lambda () 
	(. System.Console WriteLine "")))


(defn (hefty-computation do-other-stuff) 
    (letrec ((loop (lambda (n)  
      (display "Hefty computation: ") 
      (display n) 
      (newline) 
      (set! do-other-stuff (call/cc do-other-stuff)) 
      (display "Hefty computation (b)")  
      (newline) 
      (set! do-other-stuff (call/cc do-other-stuff)) 
      (display "Hefty computation (c)") 
      (newline) 
      (set! do-other-stuff (call/cc do-other-stuff)) 
      (if (> n 0) 
          (loop (- n 1)))))) (loop 5))) 




;; notionally displays a clock 
(defn (superfluous-computation do-other-stuff) 
    (letrec ((loop ( lambda () 
      (for-each (lambda (graphic) 
                  (display graphic) 
                  (newline) 
                  (set! do-other-stuff (call/cc do-other-stuff))) 
                '("Straight up." "Quarter after." "Half past."  "Quarter til.")) 
      ))) (loop ()))) 
	  
	  
(hefty-computation superfluous-computation) 	  