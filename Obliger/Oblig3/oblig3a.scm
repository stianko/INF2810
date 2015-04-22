(require r5rs)
(print-as-expression #f)
(print-mpair-curly-braces #f)
(load "prekode3a.scm")

;; Stian Kongsvik (stiako) & Mathias Källström (mathiapk)
;; Oblig 3a

;; Oppgave 1
;; (a & b)
(define original-table (make-table))

(define (mem call proc)
  (let ((table (make-table)))
    (define (memoize)
      (let ((new-proc
	     (lambda args
	       (let ((alrdy-computed (lookup args table)))
		 (or alrdy-computed
		     (let ((result (apply proc args)))
		       (insert! args result table)
		       result))))))
	(insert! new-proc proc original-table)
	new-proc))
    (define (unmemoize)
      (lookup proc original-table))
    (cond ((equal? call 'memoize)(memoize))
	  ((equal? call 'unmemoize)(unmemoize))
	  (else (display "Invalid procedure call")))))


;; (c)
;; Hovedproblemet her er at mem-fib kun memoiserer resultatet til selve kall-argumentet,
;; men ikke "resten" av tallene som brukes for å kalkulere resultatet. Dette skjer fordi
;; det brukes "define" fremfor "set!". Når man bruker "define" så endres man ikke det
;; fib spesifikt peker på, og derfor blir kun selve resultatet til kall-argumentet lagret
;; fremfor resultatet ved hver iterasjon.

;; (d)

(define (arg-finder key args)
  (cond ((null? args) '())
	((equal? key (car args))(cadr args))
	(else (arg-finder key (cddr args)))))

(define (greet . args)
  (define time (arg-finder 'time args))
  (if (null? time) (set! time "day"))
  (define title (arg-finder 'title args))
  (if (null? title) (set! title "friend"))
  (display "good ")(display time) (display " ") (display title)
  (newline))
    
;; Oppg. 2

;; (a)

(define (list-to-stream list)
  (if (null? list)'()
      (cons-stream (car list) (list-to-stream (cdr list)))))

(define (stream-to-list stream . n)
  (define (stream-iter x end)
    (cond ((= end 0)'())
	  ((null? x)'())
	  (else (cons (stream-car x)
		      (stream-iter (stream-cdr x) (- end 1))))))
  (stream-iter stream (if (null? n) 15 (car n))))

;; (b)

(define (stream-map proc . argstreams)
  (if (check-for-empty argstreams)
      the-empty-stream
      (<???>
       (apply proc (map <???> argstreams))
       (apply stream-map
	      (cons proc (map <???> argstreams))))))

(define (check-for-empty streams)
  (cond ((null? streams) #f)
	((stream-null? (car streams)))
	        (else (check-for-empty (cdr streams)))))
	  
