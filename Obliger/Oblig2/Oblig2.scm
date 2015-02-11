(require r5rs)
;; Innlevering 1b - INF2810
;; Stian Kongsvik (stiako)
;;
;; Oppg. 1
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
