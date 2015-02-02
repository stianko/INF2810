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
;; Denne evalueres til "piff!"
;;
;;(and (= 1 2)
;; "piff!"
;; "paff!"
;; (zero? (1 - 1)))
;;
;; Denne evalueres til #f.
;;
;; (if (positive? 42)       |
;;   "poff!"                |
;;   (i-am-undefined))      |
;;
;; Denne evalueres til "poff!"