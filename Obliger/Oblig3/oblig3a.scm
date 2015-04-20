(require r5rs)
(print-as-expression #f)
(print-mpair-curly-braces #f)
(load "prekode3a.scm")

;;Stian Kongsvik (stiako) & Mathias Källström (mathiapk)
;;Oblig 3a

;;Oppgave 1
;;(a & b)
(define original-table (make-table))

(define (mem call proc)
  (let ((table (make-table))
	(original-proc proc))
    (define (memoize)
      (let ((new-proc
	     (lambda args
	       (let ((alrdy-computed (lookup args table)))
		 (or alrdy-computed
		     (let ((result (apply proc args)))
		       (insert! args result table)
		       result))))))
	(insert! new-proc original-proc original-table)
	new-proc))
    (define (unmemoize)
      (lookup proc original-table))
    (cond ((equal? call 'memoize)(memoize))
	  ((equal? call 'unmemoize)(unmemoize))
	            (else (display "Invalid procedure call")))))
