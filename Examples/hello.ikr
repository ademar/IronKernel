
(define my-environment
   (bindings->environment
       (msg "Hello,world!")
       (say (lambda (x) (. System.Console WriteLine x)))))

(remote-eval (say msg) my-environment)
