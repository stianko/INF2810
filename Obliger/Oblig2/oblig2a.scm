;;Stian Kongsvik(stiako), Mathias Källström(mathiapk)

(load "huffman.scm")
;; Oppgave 1
;; (a)

(define (p-cons x y)
  (lambda (proc) (proc x y)))

(define (p-car proc)
  (proc (lambda (x y) x)))

(define (p-cdr proc)
  (proc (lambda (x y) y)))

;;
;; (b)

(define foo 30)

((lambda (x y)
   (+ foo x y))
 foo 20)
;; Dette uttrykket evaluerer til 80

((lambda (foo)
 ((lambda (x y)
    (+ foo x y))
  foo 20))
 10)
;; Dette uttrykket evaluerer til 40. I dette uttrykket benytter vi
;; en ny lokal variabel "foo", fremfor den globale vi definerte foerst.
;; Dermed er baade "foo" og "x" lik 10: 10 + 10 + 20 = 40.
;;
;; (c)
;;
;; Resultatet blir '(6 -4 21 1/2).
;; 
;; ((λ (x y z) (y x z)) (car a1) (car a2) (car a3))
;; alt. ((λ (x y z) (y x z)) 1 + 5)
;; ----------------------------------------------------------------------
;; En eller annen smart kommentar om infixnotasjon
;;
;; Oppgave 2
;; (a)

(define (member? elm lst)
  (cond ((null? lst) #f)
	((eq? elm (car lst)) #t)
	(else (member? elm (cdr lst)))))

;; (b)
;; --------------------------Sjekk denne!!!----------------------------------------
;; Den ytre decode prosedyren tar i mot en liste med bits (0,1) pluss et Huffmantre.
;; Decode-1 tar i mot resten av listen med bits og den nåværende posisjonen i treet.
;; Grunnen til at vi trenger Decode-1 er for å dele treet opp i subtrær, og for å
;; ha muligheten til å hoppe til toppen av treet når den finner "leaf".
;; --------------------------Sjekk denne!!!----------------------------------------

;; (c)

(define (decode-tail-rec bits tree)
  (define (decode-tail-1 bits current-branch finalmsg)
    (if (null? bits)
	finalmsg
	(let ((next-branch
	       (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (decode-tail-1 (cdr bits) tree (cons (symbol-leaf next-branch) finalmsg))
	      (decode-tail-1 (cdr bits) next-branch finalmsg)))))
  (decode-tail-1 bits tree '()))


(decode-tail-rec sample-code sample-tree)

;; (d)

;; Resultatet blir '(night by ninjas fight ninjas), som er den originale
;; beskjeden reversert.

;; (e)      

(define (encode msg tree)
  (if (null? msg)
      '()
      (append (encode-symbol (car msg) tree)
	      (encode (cdr msg) tree))))

(define (encode-symbol symbol tree)
(if (leaf? tree)
    '()
    (let ((current-branch (encode-branch symbol tree)))
      (cons (car current-branch)
	    (encode-symbol symbol (cadr current-branch))))))

(define (encode-branch symbol tree)
  (let ((left (left-branch tree))
	(right (right-branch tree)))
    (cond ((member? symbol (symbols left)) (list 0 left))
	  ((member? symbol (symbols right)) (list 1 right))
	  (else (error "Symbolet finnes ikke i kodeboken")))))

;; (f)

(define (grow-huffman-tree freqs)
  
