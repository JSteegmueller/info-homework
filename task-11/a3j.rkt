;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname a3) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Vorgegebene Definitionen: Blatt 11 vom 18.1.19
; Aufgabe 3: Suchbäume


; Ein Knoten (node) besteht aus
; - einem linken Zweig (left-branch)
; - einer Markierung (label) und
; - einem rechten Zweig (right-branch)
(: make-node (%a %b %c -> (node-of %a %b %c)))
(: node-left-branch ((node-of %a %b %c) -> %a))
(: node-label ((node-of %a %b %c) -> %b))
(: node-right-branch ((node-of %a %b %c) -> %c))

(define-record-procedures-parametric node node-of
  make-node
  node?
  (node-left-branch
   node-label
   node-right-branch))


; Ein leerer Baum (empty-tree) besitzt
; keine weiteren Eigenschaften
(: make-empty-tree (-> empty-tree))

(define-record-procedures empty-tree
  make-empty-tree
  empty-tree?
  ())


; Signatur für Binärbäume (btree-of t) mit Markierungen der Signatur t
; (im linken/rechten Zweig jedes Knotens findet sich jeweils wieder
; ein Binärbaum)
(define btree-of
  (lambda (t)
    (signature (mixed empty-tree
                      (node-of (btree-of t) t (btree-of t))))))
;                              \__________/   \__________/
;                                  ↑               ↑
;                                 zweifache Rekursion, s. (list-of t)



; Erzeuge einen leeren Baum
(: the-empty-tree empty-tree)
(define the-empty-tree (make-empty-tree))


; Erzeuge einen Blattknoten, d.h. beide Zweige sind je ein leerer Baum
(: make-leaf (%a -> (btree-of %a)))
(define make-leaf
  (lambda (x)
    (make-node the-empty-tree x the-empty-tree)))


; Falte Baum t bzgl. z und c
(: btree-fold (%b (%b %a %b -> %b) (btree-of %a) -> %b))

(check-expect (btree-fold 5 (lambda (left label right) 5) the-empty-tree) 5) 
(check-expect (btree-fold 1 (lambda (left label right) (+ left right)) (make-leaf "l1")) 2) 
(check-expect (btree-fold 1 (lambda (left label right) (+ left right)) (make-node (make-leaf "l1") "n1" (make-leaf "l2"))) 4)

(define btree-fold
  (lambda (z c t)
    (match t
      ((make-empty-tree) z)
      ((make-node left label right)
       (c (btree-fold z c left)
          label
          (btree-fold z c right))))))



; Exemplarische Suchbäume

; Korrekter Suchbaum
(: t1 (btree-of integer))
(define t1
  (make-node (make-node (make-leaf -4)
                        1
                        (make-leaf 3))
             12
             (make-node (make-leaf 13)
                        16
                        (make-leaf 20))))

; Kaputter Suchbaum (-4 < 1 und rechts(1) = -4)
(: t2 (btree-of integer))
(define t2
  (make-node (make-node (make-leaf 10)
                        1
                        (make-leaf -4))
             12
             (make-node (make-leaf 13)
                        16
                        (make-leaf 200))))

; Kaputter Suchbaum (10 doppelt)
(: t3 (btree-of integer))
(define t3
  (make-node (make-node (make-leaf -4)
                        1
                        (make-leaf 10))
             12
             (make-node (make-leaf 10)
                        16
                        (make-leaf 200))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Eigene Lösung ab hier

; (a) Dieses Prädikat testet Suchbäume auf ihre richtigkeit

(: search-tree? ((btree-of real) -> boolean))
(check-expect (search-tree? t1) #t)
(check-expect (search-tree? t2) #f)
;(check-expect (search-tree? t3) #f)

(define search-tree?
  (lambda (t)
    (match t
      ((make-empty-tree) #t)
      ((make-node left label right)
       (and (< (if (node? left)(node-label left) -inf.0) label (if (node? right)(node-label right) +inf.0))
            (search-tree? left) (search-tree? right))))))

(define searchtree-of
  (lambda (t)
    (signature (combined (btree-of t)
                         (predicate search-tree?)))))



; Diese Prozedur prüft ob ein Label Element eines Suchbaumes ist
(: searchtree-member? (integer (searchtree-of integer) -> boolean))
(check-expect (searchtree-member? 3 t1)  #t)
(check-expect (searchtree-member? 16 t1) #t)
(check-expect (searchtree-member? 42 t1) #f)

(define searchtree-member?
  (lambda (num t)
    (match t
      ((make-empty-tree) #f)
      ((make-node l x r) (if (= num x) #t
                             (if (< num x) (searchtree-member? num l)
                                 (searchtree-member? num r)))))))


; Diese Prozedur fügt eine Zahl in einen Suchbaum ein
(: searchtree-insert (integer (searchtree-of integer)-> (searchtree-of integer)))
(check-expect (searchtree-insert 3 t1) t1)
(check-expect (searchtree-insert 1 the-empty-tree) (make-leaf 1))
(check-expect (searchtree-insert 300 t1) (make-node (make-node (make-leaf -4)
                                                               1
                                                               (make-leaf 3))
                                                    12
                                                    (make-node (make-leaf 13)
                                                               16
                                                               (make-node the-empty-tree 20 (make-leaf 300) ))))


(define searchtree-insert
  (lambda (num t)
    (cond
      ((empty-tree? t) (make-leaf num))
      ((searchtree-member? num t) t)
      (else (match t
              ((make-node l x r) (if (< num x) (make-node (searchtree-insert num l) x r)
                                     (make-node l x (searchtree-insert num r)))))))))


; Diese Prozedur wandelt eine Liste in einen Suchbaum
(: list->searchtree ((list-of integer)-> (searchtree-of integer)))
(check-expect (list->searchtree (list -4 3 1 13 20 16 12)) t1)

(define list->searchtree
  (lambda (xs)
    (fold the-empty-tree searchtree-insert xs)))








 