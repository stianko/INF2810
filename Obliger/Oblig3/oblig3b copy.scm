;;(require r5rs)
;;(print-as-expression #f)
;;(print-mpair-curly-braces #f)
(load "evaluator.scm")
(load "oblig3b.scm")
(set! the-global-environment (setup-environment))
(define global the-global-environment)
(define repl read-eval-print-loop)
(repl)

;; ((lambda (x y z) (+ x y z)) 1 3 9)
;; (let ((x 1) (y 3) (z 9)) (+ x y z))
;; (let x = 1 and y = 3 and z = 9 in (+ x y z))