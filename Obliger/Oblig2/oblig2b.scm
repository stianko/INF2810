(require r5rs)
(print-as-expression #f)
(print-mpair-curly-braces #f)
(load "oblig2b_tester.scm")

;; 
;; Oblig 2b, INF2810 v15
;;
;; Oppg. 1
;; (a)

(define (make-counter)
  (define count 0)
    (λ ()
      (begin (set! count (+ count 1))
	     count)))
;;
;; (b)
;; -------------------------------------------------------------
;; -----------------------Fancy diagram-------------------------
;; Sjekk side 242 i SICP, dette gidder vi ikke
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

(define (pop! stack)
  (stack 'pop!))

(define (stack stack-object)
  (stack-object 'stack))

(define (push! stack-object . r)
  (apply stack-object (cons 'push! r)))
  
;; Oppg. 3
;; (a)

;; BOKS-OG-PEKER DIAGRAM

;; list-ref returnerer symbolet i en liste på en gitt indeks.
;; I dette eksempelet vil list-ref kall med indeks 0-3 gi henholdsvis a-d.
;; Men fordi cdddr (d e) blir satt til å peke på cdr (b c d e) blir e "borte".
;; Dette er fordi hver gang vi kommer til d i lista vil den peke tilbake til b, 
;; og vi kommer aldri til e. Dermed får vi at et kall på list-ref med indeks 1, 4, 7, 10 (++3)
;; vil alle returnere b.

;; (b)

;; BOKS-OG-PEKER DIAGRAM

;; Etter første kallet på set-car! får vi en nested list. 
;; Når vi da kaller set-car! på den indre listen i bah (car bah) 
;; setter vi kun car av den indre listen til å peke på 42, 
;; ikke car av bah. 

;; (c)

(define (cycle1? list)
  (let ((templist '()))
    (define (list-iter rest-list)
      (cond ((null? rest-list) #f)
	    ((memq (car rest-list) templist) #t)
	   ;; ((list? list)#f)
	    (else (set! templist (cons (car rest-list) templist))
		  (list-iter (cdr rest-list)))))
    (list-iter list)))

(define (cycle? list)
  (define (list-iter rest-list counted)
    (cond ((null? rest-list) #f)
	  ((memq rest-list counted) #t)
	  (else (list-iter (cdr rest-list) (cons rest-list counted)))))
  (list-iter list '()))
