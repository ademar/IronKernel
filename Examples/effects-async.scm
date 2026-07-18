; Tagged deep handlers and CLR Task suspension.

(define request (make-prompt-tag))

(define handled
  (prompt request
    (lambda (value resume-request)
      (resume resume-request (+ value 1)))
    (+ 1 (perform request 40))))

(printf "handled={0}\n" handled)

(define delayed
  (await-task (task-delay 25 "async complete")))

(Console.write-line delayed)
