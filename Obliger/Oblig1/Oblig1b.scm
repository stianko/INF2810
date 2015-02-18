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
(cons (cons 1 (cons 2 '())) (cons(cons 3 (cons 4 '())) '()))
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
;; Kommentar "som forklarer hvorvidt du har brukt “vanlig” rekursjon eller halerekursjon. Beskriv og
;; så hvordan den tilsvarende prosessens ressursbehov (tid/minne) vokser relativt til størrelsen på
;; input."
;;
;; (d)
(define (nth n items)
