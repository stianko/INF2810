;;(require r5rs)
;;(print-as-expression #f)
;;(print-mpair-curly-braces #f)
(load "evaluator.scm")
(load "oblig3b.scm")
(set! the-global-environment (setup-environment))
(define global the-global-environment)
(define repl read-eval-print-loop)