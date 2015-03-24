(require r5rs)
(print-as-expression #f)
(print-mpair-curly-braces #f)

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
;;
;; (b)
;; -------------------------------------------------------------
;; -----------------------Fancy diagram-------------------------
;; -------------------------------------------------------------

;; Oppg. 2
;; (a)

(define (make-stack stack)
  (define (push! list)
    (set! stack (append (reverse list) stack)))
  (define (pop!)
    (if (not (null? stack))
    (set! stack (cdr stack))))
  (define (pass-message message . r)
      (cond ((eq? message 'push!) (push! r))
	    ((eq? message 'pop!) (pop!))
	    ((eq? message 'stack) stack)))
  pass-message)

;; (b)

(define (push! stack-object . r)
  (if (null? r)
      '()
      (stack-object 'push! (car r)))
  (display r))
      ;;(push! stack-object (cdr r))))

(define (pop! stack)
  (stack 'pop!))

(define (stack stack-object)
  (stack-object 'stack))
  
