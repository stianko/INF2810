(require r5rs)
;; Innlevering 1a - INF2810
;; Stian Kongsvik (stiako)

;; Oppg. 1
;; (a)
;; (* (+ 2 2) 5) - Her vil først (+ 2 2) evalueres til 4, så (* 4 5) til 20
;;
;; (b)
;; (* (+ 2 2) (5)) - Kjører ikke, ettersom 5 ikke er en operator
;;
;; (c)
;; (* (2 + 2) 5) - Kjører ikke, ettersom infiksnotasjon ikke fungerer, som i forrige, fordi et tall ikke er en operator
;;
;; (d)
;; (define bar (/ 4 2)) - Her setter vi navnet "bar" på en variabel som har verdi (/ 4 2), altså 2.
;; bar - returnerer 2
;;
;; (e)
;; (- bar 2) - Her evalueres (- 2 2) som blir 0
;;
;; (f)
;; (/ (* bar 3 4 1) bar) - Her evalueres først (* 2 3 4 1) som blir 24, så (/ 24 2) som blir 12
;;
;; Oppg. 2
;; (a)
;; (or (= 1 2)
;;   "piff!" 
;;   "paff!"
;;   (zero? (1 - 1)))
;;
;; Denne evalueres til "piff!". Dette fordi "or" vil evaluere til den kommer til det første sanne tilfellet.
;; Det er også en syntaksfeil på den siste linjen (1 - 1).
;;
;; (and (= 1 2)
;;  "piff!"
;;  "paff!"
;;  (zero? (1 - 1)))
;;
;; Denne evalueres til #f, fordi det første tilfellet ikke er sant. Den innholder i tillegg samme syntaksfeil i siste linje som på
;; forrige prosedyre.
;;
;; (if (positive? 42)
;;   "poff!"
;;   (i-am-undefined))
;;
;; Denne evalueres til "poff!", fordi predikatet (positive? 42) er sant. Kun "poff!", altså konsekvensen, blir evaluert.
;;
;; Alle tre prosedyrene er special forms fordi de ikke evaluerer alle klausulene likt som på generell form.
;;
;; (b)
(define (sign x)
  (if (< x 0)
      -1
      (if (= x 0)
	  0
	  (if (> x 0)
	      1))))

(define (sign x)
  (cond ((< x 0) -1)
	((= x 0) 0)
	((> x 0) 1)))

;; (c)
(define (sign x)
  (or
   (and (< x 0)
	-1)
   (and (= x 0)
	0)
   (and (> x 0)
	1)))

;; Oppg. 3
;; (a)
(define (add1 x)
  (+ x 1))
(define (sub1 x)
  (- x 1))

;; (b)
(define (plus x y)
  (if (zero? y)
      x
      (add1 (plus x (sub1 y)))))
;; (c)
;; Metoden i (b) er en rekursiv prosess av en rekursiv prosedyre.
;; Rekursjonsstegen er vist her:
;; (add1 (plus 2 (sub1 3))
;; (add1 (plus 2 2))
;; (add1 (add1 (plus 2 (sub1 2))))
;; (add1 (add1 (plus 2 1)))
;; (add1 (add1 (add1 (plus 2 (sub1 1)))))
;; (add1 (add1 (add1 (plus 2 0))))
;; -------------------------------------------------
;;  y er null, og prosessen slutter å kalle seg selv
;; -------------------------------------------------
;; (add1 (add1 (add1 2)))
;; (add1 (add1 3))
;; (add1 (4))
;; 5
;;
;; Halerekursiv - iterativ prosess av rekursiv prosedyre:
(define (plus x y)
  (if (zero? y)
      x
      (plus (add1 x) (sub1 y))))
;; De iterative stegene:
;; (plus 2 3)
;; (plus 3 2)
;; (plus 4 1)
;; (plus 5 0)
;; 5
;;
;; (d)

(define (power-close-to b n)
  (power-iter b n 1))
(define (power-iter b n e)
  (if (> (expt b e) n)
      e
      (power-iter b n (+ 1 e))))

;; Forenklet med blokkstruktur:

(define (power-close-to b n)
  (define (power-iter e)
    (if (> (expt b e) n)
	e
	(power-iter (+ 1 e))))
    (power-iter 1))

;; Den er også forenklet med at (power-iter e) kun tar med ett parameter. Dette kan vi gjøre fordi
;; b og n er frie parametre i den indre definisjonen av prosedyren.
