(require r5rs)
(print-as-expression #f)
(print-mpair-curly-braces #f)
(load "prekode3a.scm")

;;Stian Kongsvik (stiako) & Mathias Källström (mathiapk)
;;Oblig 3a

;;Oppgave 1
;;(a & b)

(define (mem call proc)
  (let ((table (make-table)))
    (define (memoize)
      (lambda args
	(let ((alrdy-computed (lookup args table)))
	  (or alrdy-computed
	      (let ((result (apply proc args)))
		(insert! args result table)
		result)))))
    (define (unmemoize)
      backup)
    (cond ((equal? call 'memoize)(memoize))
	  ((equal? call 'unmemoize)(unmemoize))
	  (else (display "Invalid procedure call")))))




