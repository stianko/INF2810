(require r5rs)
;; 
;; Oblig 2b, INF2810 v15
;;
;; Oppg. 1
;; (a)
(define make-counter
  (lambda ()
    (define count 0)
    (lambda ()
      (begin (set! count (+ count 1))
	     count))))
