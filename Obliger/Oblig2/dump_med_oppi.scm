Welcome to Racket v6.1.1.
racket@> (huffman-leaves-3 samurai-tree)
'((((night 12) ((hide 3) (defeat 1) river 2) (sword 4) (ambush 2) in 2)
   (samurais 20)
   ((((poison 1) wait 1) (forest 1) assassin 1) the 5)
   by
   12)
  (fight 45)
  ninjas
  57)
racket@> (huffman-leaves-3 samurai-tree)
'(night
  12
  hide
  3
  defeat
  1
  river
  2
  sword
  4
  ambush
  2
  in
  2
  samurais
  20
  poison
  1
  wait
  1
  forest
  1
  assassin
  1
  the
  5
  by
  12
  fight
  45
  ninjas
  57)
racket@> (huffman-leaves-3 samurai-tree)
'((((night 12)
    (((hide 3) ((defeat 1) (river 2))) ((sword 4) ((ambush 2) (in 2)))))
   ((samurais 20)
    (((((poison 1) (wait 1)) ((forest 1) (assassin 1))) (the 5)) (by 12))))
  ((fight 45) (ninjas 57)))
racket@> (huffman-leaves-3 samurai-tree)
'((night 12)
  (hide 3)
  (defeat 1)
  (river 2)
  (sword 4)
  (ambush 2)
  (in 2)
  (samurais 20)
  (poison 1)
  (wait 1)
  (forest 1)
  (assassin 1)
  (the 5)
  (by 12)
  (fight 45)
  (ninjas 57))
racket@> (huffman-leaves-3 sample-tree)
'((ninjas 8) (fight 5) (night 1) (by 1))
racket@> (huffman-leaves sample-tree)
'((ninjas 8) (fight 5) (night 1) (by 1))
racket@> (huffman-leaves samurai-tree)
'((night 12)
  (hide 3)
  (defeat 1)
  (river 2)
  (sword 4)
  (ambush 2)
  (in 2)
  (samurais 20)
  (poison 1)
  (wait 1)
  (forest 1)
  (assassin 1)
  (the 5)
  (by 12)
  (fight 45)
  (ninjas 57))
racket@> (huffman-leaves samurai-tree)
'((night 12)
  (hide 3)
  (defeat 1)
  (river 2)
  (sword 4)
  (ambush 2)
  (in 2)
  (samurais 20)
  (poison 1)
  (wait 1)
  (forest 1)
  (assassin 1)
  (the 5)
  (by 12)
  (fight 45)
  (ninjas 57))
racket@> (huffman-leaves samurai-tree)
'((night 12)
  (hide 3)
  (defeat 1)
  (river 2)
  (sword 4)
  (ambush 2)
  (in 2)
  (samurais 20)
  (poison 1)
  (wait 1)
  (forest 1)
  (assassin 1)
  (the 5)
  (by 12)
  (fight 45)
  (ninjas 57))
racket@> sample-tree
'((leaf ninjas 8)
  ((leaf fight 5) ((leaf night 1) (leaf by 1) (night by) 2) (fight night by) 7)
  (ninjas fight night by)
  15)
racket@> (symbol-weight-list '(leaf ninjas 8))
'((ninjas 8))
racket@> (weight samurai-tree)
169
racket@> (length samurai-tree)
4
racket@> (length (huffman-leaves samurai-tree))
16
racket@> (symbols samurai-tree)
'(night
  hide
  defeat
  river
  sword
  ambush
  in
  samurais
  poison
  wait
  forest
  assassin
  the
  by
  fight
  ninjas)
racket@> (caadr (huffman-leaves sample-tree))
'fight
racket@> (car (huffman-leaves sample-tree))
'(ninjas 8)
racket@> (cadr (huffman-leaves sample-tree))
'(fight 5)
racket@> (caar (huffman-leaves sample-tree))
'ninjas
racket@> (cadar (huffman-leaves sample-tree))
8
racket@> (cadr '(a 2))
2
racket@> (encode-symbol 'samurai samurai-tree)
Symbolet finnes ikke i kodeboken
  context...:
   encode-symbol
   /Applications/Racket/collects/racket/private/misc.rkt:87:7
racket@> (encode-symbol 'samurais samurai-tree)
'(0 1 0)
racket@> (length (encode-symbol 'samurais samurai-tree))
3
racket@> (map encode-symbol (symbols sample-tree) (samurai-tree))
application: not a procedure;
 expected a procedure that can be applied to arguments
  given: '((((leaf night 12) (((leaf hide 3) ((leaf defeat 1) (leaf river 2) (defeat river) 3) (hide defeat river) 6) ((leaf sword 4) ((leaf ambush 2) (leaf in 2) (ambush in) 4) (sword ambush in) 8) (hide defeat river sword ambush in) 14) (night hide defeat rive...
  arguments...: [none]
  context...:
   /Applications/Racket/collects/racket/private/misc.rkt:87:7
racket@> (map (lambda (x y) (encode-symbol x y)) (symbols sample-tree) (samurai-tree))
application: not a procedure;
 expected a procedure that can be applied to arguments
  given: '((((leaf night 12) (((leaf hide 3) ((leaf defeat 1) (leaf river 2) (defeat river) 3) (hide defeat river) 6) ((leaf sword 4) ((leaf ambush 2) (leaf in 2) (ambush in) 4) (sword ambush in) 8) (hide defeat river sword ambush in) 14) (night hide defeat rive...
  arguments...: [none]
  context...:
   /Applications/Racket/collects/racket/private/misc.rkt:87:7
racket@> (map (lambda (x y) (encode-symbol x y)) (symbols sample-tree) samurai-tree)
Symbolet finnes ikke i kodeboken
  context...:
   encode-symbol
   /Applications/Racket/collects/racket/private/map.rkt:37:19: loop
   /Applications/Racket/collects/racket/private/misc.rkt:87:7
racket@> (map (lambda (x y) (encode-symbol x y)) (symbols sample-tree) sample-tree)
car: contract violation
  expected: pair?
  given: 'ninjas
  context...:
   /Users/stian/repos/INF2810/Obliger/Oblig2/huffman.scm:9:0: leaf?
   /Users/stian/repos/INF2810/Obliger/Oblig2/huffman.scm:26:0: symbols
   encode-branch
   encode-symbol
   /Applications/Racket/collects/racket/private/map.rkt:37:19: loop
   /Applications/Racket/collects/racket/private/misc.rkt:87:7
racket@> (symbols sample-tree)
'(ninjas fight night by)
racket@> sample-tree
'((leaf ninjas 8)
  ((leaf fight 5) ((leaf night 1) (leaf by 1) (night by) 2) (fight night by) 7)
  (ninjas fight night by)
  15)
racket@> (map (lambda (x y) (encode-symbol x y)) 'ninjas sample-tree)
map: contract violation
  expected: list?
  given: 'ninjas
  argument position: 2nd
  other arguments...:
   #<procedure>
   '((leaf ninjas 8) ((leaf fight 5) ((leaf night 1) (leaf by 1) (night by) 2) (fight night by) 7) (ninjas fight night by) 15)
  context...:
   /Applications/Racket/collects/racket/private/misc.rkt:87:7
racket@> (map (lambda (x y) (encode-symbol x y)) '(ninjas) sample-tree)
map: all lists must have same size; arguments were: #<procedure> '(ninjas) '((leaf ninjas 8) ((leaf fight 5) ((leaf night 1) (leaf by 1) (night by) 2) (fight...
  context...:
   /Applications/Racket/collects/racket/private/misc.rkt:87:7
racket@> (length sample-tree)
4
racket@> (map (lambda (x y) (encode-symbol x y)) '(ninjas fight by nigh) sample-tree)
car: contract violation
  expected: pair?
  given: 'ninjas
  context...:
   /Users/stian/repos/INF2810/Obliger/Oblig2/huffman.scm:9:0: leaf?
   /Users/stian/repos/INF2810/Obliger/Oblig2/huffman.scm:26:0: symbols
   encode-branch
   encode-symbol
   /Applications/Racket/collects/racket/private/map.rkt:37:19: loop
   /Applications/Racket/collects/racket/private/misc.rkt:87:7
racket@> (map (lambda (x y) (encode-symbol x y)) '(ninjas fight by night) sample-tree)
car: contract violation
  expected: pair?
  given: 'ninjas
  context...:
   /Users/stian/repos/INF2810/Obliger/Oblig2/huffman.scm:9:0: leaf?
   /Users/stian/repos/INF2810/Obliger/Oblig2/huffman.scm:26:0: symbols
   encode-branch
   encode-symbol
   /Applications/Racket/collects/racket/private/map.rkt:37:19: loop
   /Applications/Racket/collects/racket/private/misc.rkt:87:7
racket@> (map (lambda (x y) (encode-symbol x y)) (caar leaves) sample-tree)
car: contract violation
  expected: pair?
  given: 'ninjas
  context...:
   /Users/stian/repos/INF2810/Obliger/Oblig2/huffman.scm:9:0: leaf?
   /Users/stian/repos/INF2810/Obliger/Oblig2/huffman.scm:26:0: symbols
   encode-branch
   encode-symbol
   /Applications/Racket/collects/racket/private/map.rkt:37:19: loop
   /Applications/Racket/collects/racket/private/misc.rkt:87:7
racket@> (encode-symbol 'ninjas sample-tree)
'(0)
racket@> (map (lambda (x y) (encode-symbol x y)) (caar '((ninjas 8) (fight 5) (night 1)) sample-tree)
	   )
caar: arity mismatch;
 the expected number of arguments does not match the given number
  expected: 1
  given: 2
  arguments...:
   '((ninjas 8) (fight 5) (night 1))
   '((leaf ninjas 8) ((leaf fight 5) ((leaf night 1) (leaf by 1) (night by) 2) (fight night by) 7) (ninjas fight night by) 15)
  context...:
   /Applications/Racket/collects/racket/private/misc.rkt:87:7
racket@> (map (lambda (x) (encode-symbol x sample-tree)) (symbols sample-tree))
'((0) (1 0) (1 1 0) (1 1 1))
racket@> (reduce + 0 (map (lambda (x) (encode-symbol x sample-tree)) (symbols sample-tree)))
reduce: undefined;
 cannot reference undefined identifier
  context...:
   /Applications/Racket/collects/racket/private/misc.rkt:87:7
racket@> (reduce + 0 '((0) (1 0) (1 1 0)))
reduce: undefined;
 cannot reference undefined identifier
  context...:
   /Applications/Racket/collects/racket/private/misc.rkt:87:7
racket@> (cdr (a 1))
a: undefined;
 cannot reference undefined identifier
  context...:
   /Applications/Racket/collects/racket/private/misc.rkt:87:7
racket@> (cdr '(a 1))
'(1)
racket@> (reduce (lambda (x) (length x)) (map (lambda (x) (encode-symbol x sample-tree)) (symbols sample-tree)))
reduce: undefined;
 cannot reference undefined identifier
  context...:
   /Applications/Racket/collects/racket/private/misc.rkt:87:7
racket@> (map (lambda (x) (encode-symbol x sample-tree)) (symbols sample-tree))
'((0) (1 0) (1 1 0) (1 1 1))
racket@> (reduce (lambda (x) (length x)) 0 (map (lambda (x) (encode-symbol x sample-tree)) (symbols sample-tree)))
reduce: undefined;
 cannot reference undefined identifier
  context...:
   /Applications/Racket/collects/racket/private/misc.rkt:87:7
racket@> (map (lambda (x) (encode-symbol x sample-tree)) (symbols sample-tree))
'((0) (1 0) (1 1 0) (1 1 1))
racket@> (map (lambda (x) (length (encode-symbol x sample-tree))) (symbols sample-tree))
'(1 2 3 3)
racket@> (reduce + 0(map (lambda (x) (length (encode-symbol x sample-tree))) (symbols sample-tree)))
reduce: undefined;
 cannot reference undefined identifier
  context...:
   /Applications/Racket/collects/racket/private/misc.rkt:87:7
racket@> (reduce + 0 (map (lambda (x) (length (encode-symbol x sample-tree))) (symbols sample-tree)))
reduce: undefined;
 cannot reference undefined identifier
  context...:
   /Applications/Racket/collects/racket/private/misc.rkt:87:7
racket@> (reduce + 0 '(1 2 3 3))
reduce: undefined;
 cannot reference undefined identifier
  context...:
   /Applications/Racket/collects/racket/private/misc.rkt:87:7
racket@> (reduce + 0 '(1 2 3 3))
reduce: undefined;
 cannot reference undefined identifier
  context...:
   /Applications/Racket/collects/racket/private/misc.rkt:87:7
racket@> (define (reduce proc init items)
	       (if (null? items)
		           init
			           (proc (car items)
					               (reduce proc init (cdr items)))))
racket@> (reduce + 0 '(1 2 3 3))
9
racket@> (reduce + 0 (map (lambda (x) (length (encode-symbol x sample-tree))) (symbols sample-tree)))
9
racket@> (define bit-list (map (lambda (x) (encode-symbol x sample-tree)) (symbols sample-tree)))
racket@> (reduce + 0 (map (λ (x y) (avg-codeword-length x sample-tree y)) (symbols sample-tree) bit-list))
avg-codeword-length: undefined;
 cannot reference undefined identifier
  context...:
   /Applications/Racket/collects/racket/private/misc.rkt:87:7
racket@> (reduce + 0 (map (λ (x y) (avg-codeword-length x sample-tree y)) (symbols sample-tree) bit-list))
symbol-weight: undefined;
 cannot reference undefined identifier
  context...:
   /Applications/Racket/collects/racket/private/map.rkt:37:19: loop
   /Applications/Racket/collects/racket/private/misc.rkt:87:7
racket@> (reduce + 0 (map (λ (x y) (avg-codeword-length x sample-tree y)) (symbols sample-tree) bit-list))
cadr: contract violation
  expected: (cons/c any/c pair?)
  given: 'ninjas
  context...:
   symbol-weight
   avg-codeword-length
   /Applications/Racket/collects/racket/private/map.rkt:37:19: loop
   /Applications/Racket/collects/racket/private/misc.rkt:87:7
racket@> (avg-codeword-length 'ninjas sample-tree '(0))
cadr: contract violation
  expected: (cons/c any/c pair?)
  given: 'ninjas
  context...:
   symbol-weight
   avg-codeword-length
   /Applications/Racket/collects/racket/private/misc.rkt:87:7
racket@> (avg-codeword-length '(ninjas 8) sample-tree '(0))
8/15
racket@> (reduce + 0 (map (λ (x y) (avg-codeword-length x sample-tree y)) (symbols sample-tree) bit-list))
cadr: contract violation
  expected: (cons/c any/c pair?)
  given: 'ninjas
  context...:
   symbol-weight
   avg-codeword-length
   /Applications/Racket/collects/racket/private/map.rkt:37:19: loop
   /Applications/Racket/collects/racket/private/misc.rkt:87:7
racket@> (map (λ (x y) (avg-codeword-length x sample-tree y)) (huffman-leaves sample-tree) bit-list)
'(8/15 2/3 1/5 1/5)
racket@> (reduce + 0(map (λ (x y) (avg-codeword-length x sample-tree y)) (huffman-leaves sample-tree) bit-list))
8/5
racket@> (reduce + 0 (map (λ (x y) (avg-codeword-length x samurai-tree y)) (huffman-leaves samurai-tree) bit-list))
map: all lists must have same size; arguments were: #<procedure> '((night 12) (hide 3) (defeat 1) (river 2) (sword 4) (ambush 2) (in 2) (samurais 2... '((0) (1 0) (1 1 0) (1 1 1))
  context...:
   /Applications/Racket/collects/racket/private/misc.rkt:87:7
racket@> (define bit-list (map (lambda (x) (encode-symbol x sample-tree)) (symbols sample-tree)))
racket@> (define bit-list (map (lambda (x) (encode-symbol x samurai-tree)) (symbols samurai-tree)))
racket@> (reduce + 0 (map (λ (x y) (avg-codeword-length x samurai-tree y)) (huffman-leaves samurai-tree) bit-list))
478/169
racket@> 