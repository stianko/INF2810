(require r5rs)
;; Innlevering 1b - INF2810
;; Stian Kongsvik (stiako)
;;
;; Oppg. 1 - Par og lister
;; (a)
(cons 47 11)
;;
;;    +------+------+   +------+
;;    |      |      |   |      |
;; -->|   +  |   +----->|  11  |
;;    |   |  |      |   |      |
;;    +---|--+------+   +------+
;;        |
;;        v
;;    +------+
;;    |      |
;;    |  47  |
;;    |      |
;;    +------+
;;
;; (b)
(cons 47 '())
;;
;;    +-----+-----+
;;    |     |   / |
;; -->|  +  |  /  |
;;    |  |  | /   |
;;    +--|--+-----+
;;       |
;;       v
;;    +------+
;;    |      |
;;    |  47  |
;;    |      |
;;    +------+
;;
;; (c)
(list 47 11)
;;
;;    +------+------+   +------+-----+
;;    |      |      |   |      |   / |
;; -->|   +  |   +----->|   +  |  /  | 
;;    |   |  |      |   |   |  | /   |
;;    +---|--+------+   +---|--+-----+
;;        |                 |
;;        v                 v
;;    +------+          +------+
;;    |      |          |      |
;;    |  47  |          |  11  |
;;    |      |          |      |
;;    +------+          +------+
;;
;; (d)
'(47 (11 12))
;;
;;    +------+------+   +------+------+
;;    |      |      |   |      |    / |
;; -->|   +  |   +----->|   +  |   /  | 
;;    |   |  |      |   |   |  |  /   |
;;    +---|--+------+   +---|--+------+
;;        |                 |
;;        v                 v
;;    +------+          +------+------+   +------+------+
;;    |      |          |      |      |   |      |    / |
;;    |  47  |          |   +  |   +----->|   +  |   /  |
;;    |      |          |   |  |      |   |   |  |  /   |
;;    +------+          +---|--+------+   +---|--+------+
;;                          |                 |
;;                          v                 v
;;                      +------+          +------+
;;                      |      |          |      |
;;                      |  11  |          |  12  |
;;                      |      |          |      |
;;                      +------+          +------+
;;
;; (e)
(define foo '(1 2 3))
(cons foo foo)
;;
;;    +------+------+   +------+------+   +------+------+   +------+------+
;;    |      |      |   |      |      |   |      |      |   |      |    / |
;; -->|   +  |   +----->|   +  |   +----->|   +  |   +----->|   +  |   /  |
;;    |   |  |      |   |   |  |      |   |   |  |      |   |   |  |  /   |
;;    +---|--+------+   +---|--+------+   +---|--+------+   +---|--+------+
;;        |                 |                 |                 |
;;        |                 v                 v                 v
;;        |             +------+          +------+          +------+
;;        |             |      |          |      |          |      |
;;        |             |   1  |          |   2  |          |   3  |
;;        |             |      |          |      |          |      |
;;        |             +------+          +------+          +------+
;;        v
;;    +------+------+   +------+------+   +------+------+
;;    |      |      |   |      |      |   |      |    / |
;;    |   +  |   +----->|   +  |   +----->|   +  |   /  |
;;    |   |  |      |   |   |  |      |   |   |  |  /   |
;;    +---|--+------+   +---|--+------+   +---|--+------+
;;        |                 |                 |
;;        v                 v                 v
;;    +------+          +------+          +------+
;;    |      |          |      |          |      |
;;    |   1  |          |   2  |          |   3  |
;;    |      |          |      |          |      |
;;    +------+          +------+          +------+
;;
;; (f)
;; (1 2 3 4)
(caddr '(1 2 3 4))
;;
;; (g)
;; ((1 2) (3 4))
(caadr '((1 2) (3 4)))
;;
;; (h)
;; ((1) (2) (3) (4))
(caaddr '((1) (2) (3) (4)))
;;
;; (i)
(cons (cons 1 (cons 2 '())) (cons (cons 3 (cons 4 '())) '()))
(list (list 1 2) (list 3 4))
;;
;; Oppg 2 - Rekursjon over lister og høyereordens prosedyrer
;; (a)
(define (length2 items)
  (define (length-iter in out)
    (if (null? in)
	out
	(length-iter (cdr in) (+ 1 out))))
  (length-iter items 0))
;;
;; (b)
(define (rev-list items)
  (define (rev-iter in out)
    (if (null? in)
	out
	(rev-iter (cdr in)
		  (cons (car in) out))))
  (rev-iter items '()))
;;
;; I denne oppgaven har jeg valgt å bruke halerekursjon, ettersom det enkelt vil
;; lage en kopi av listen reversert, med like mange steg som antall elementer i listen.
;; Eksempel: (1 2 3 4)
;;
;;  | in        | in til neste steg | out til neste steg
;;  |           | (cdr in)          | (cons (car in) out)
;; 1| (1 2 3 4) | (2 3 4)           | (1) 
;; 2|   (2 3 4) | (3 4)             | (2 1)
;; 3|     (3 4) | (4)               | (3 2 1)
;; 4|        (4)| '()               | (4 3 2 1)
;;
;; (c)
(define (ditch x items)
  (cond ((null? items) '())
	((not (equal? x (car items)))
	 (cons (car items)
	       (ditch x (cdr items))))
	(else (ditch x (cdr items)))))
;;
;; Her har jeg brukt "vanlig" rekursjon og som følge av dette vil den først lete gjennom hele
;; listen før den vil begynne å "conse" elementene sammen. Dette gjør at den vil øke med 2x steg
;; for hvert ekstra element i listen.
;;
;; (d)
(define (nth n items)
  (define (nth-iter recitems acc)
    (if (= n acc)
	(car recitems)
	(nth-iter (cdr recitems) (+ 1 acc))))
  (nth-iter items 0))
;;
;; (e)
(define (where x items)
  (define (where-iter recitems acc)
    (cond ((null? recitems) #f)
	  ((= x (car recitems)) acc)
	  (else (where-iter (cdr recitems) (+ 1 acc)))))
    (where-iter items 0))
;;
;; (f)
(define (map2 proc list1 list2)
  (cond ((null? list1) '())
	((null? list2) '())
	(else (cons (proc (car list1) (car list2))
		    (map2 proc (cdr list1) (cdr list2))))))
;;
;; (g)
(map2 (lambda (x y) (/ (+ x y) 2)) '(1 2 3 4) '(3 4 5))
;;
(map2 (λ (x y) (cond ((odd? x) #f)
		     ((odd? y) #f)
		     (else #t)))
      '(1 2 3) '(1 2 3))
;;
;; (h)
(define (both? proc)
  (lambda (x y)
    (and (proc x) (proc y))))
;;
;; (i)
(define (self? proc)
  (lambda (x)
    (proc x x)))
