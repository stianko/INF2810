(require r5rs)
;; Oblig 1 - INF2810
;; Stian Kongsvik (stiako)
;; Oppg. 1
;; (a)
;; (* (+ 2 2) 5) - Her vil først (+ 2 2) evalueres til 4, så (* 4 5) til 20
;; (b)
;; (* (+ 2 2) (5)) - Kjører ikke, ettersom 5 ikke er en operator
;; (c)
;; (* (2 + 2) 5) - Kjører ikke, ettersom infiksnotasjon ikke fungerer, som i forrige, ettersom et tall ikke er en operator
;; (d)
;; (define bar (/ 4 2)) - Her setter vi navnet "bar" på en variabel som har verdi (/ 4 2), altså 2
;; bar
;; (e)
;; (- bar 2) - Her evalueres (- 2 2) som blir 0
;; (f)
;; (/ (* bar 3 4 1) bar) - Her evalueres først (* 2 3 4 1) som blir 24, så (/ 24 2) som blir 12
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
;;(and (= 1 2)
;; "piff!"
;; "paff!"
;; (zero? (1 - 1)))
;;
;; Denne evalueres til #f, fordi det første tilfellet ikke er sant. Det er i tillegg samme syntaksfeil i siste linje som på
;; forrige prosedyre.
;;
;; (if (positive? 42)
;;   "poff!"
;;   (i-am-undefined))
;;
;; Denne evalueres til "poff!", fordi predikatet (positive? 42) er sant. Kun "poff!", altså konsekvensen, blir evaluert.
;;
;; Alle tre prosedyrene er special forms fordi de ikke evaluerer alle klausulene likt som standard scheme.
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
(sign -0)
;; (c)
(define (sign x)
  (or
   (and (< x 0)
	-1)
   (and (= x 0)
	0)
   (and (> x 0)
	1)))

;; Oppg. 2
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
;; Halerekursiv
(define (plus x y)
  (if (zero? y)
      x
      (plus (add1 x) (sub1 y))))
