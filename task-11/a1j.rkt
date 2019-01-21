;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname a1j) (read-case-sensitive #f) (teachpacks ((lib "image2.ss" "teachpack" "deinprogramm") (lib "universe.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.ss" "teachpack" "deinprogramm") (lib "universe.ss" "teachpack" "deinprogramm")))))
; Vorgegebene Definitionen: Blatt 11 vom 18.1.19
; Aufgabe 1: Binärbäume


; Ein Knoten (node) besitzt
; - einen linken Zweig (left-branch),
; - eine Markierung (label) und
; - einen rechten Zweig (right-branch)
(: make-node (%a %b %c -> (node-of %a %b %c)))
(: node-left-branch  ((node-of %a %b %c) -> %a))
(: node-label        ((node-of %a %b %c) -> %b))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Eigene Lösung ab hier

; Diese Funktion ermittelt den minimalen Wert einer Markiereung eines B-Baumes
(: btree-min ((btree-of real) -> real))
(check-within (btree-min
               (make-node (make-node the-empty-tree 0 the-empty-tree) 2 (make-node the-empty-tree -1 the-empty-tree)))
              -1 0.1)
(check-within (btree-min (make-node the-empty-tree 2 the-empty-tree))
              2 0.1)

(define btree-min
  (lambda (tr)
    (match tr
      ((make-empty-tree) +inf.0)
      ((make-node l val r) (min (btree-min l) val (btree-min r))))))

; Diese Funktion ermittelt den maximalen Wert einer Markiereung eines B-Baumes
(: btree-max ((btree-of real) -> real))
(check-within (btree-max
               (make-node (make-node the-empty-tree 0 the-empty-tree) 2 (make-node the-empty-tree -1 the-empty-tree)))
              2 0.1)
(check-within (btree-max (make-node the-empty-tree 2 the-empty-tree))
              2 0.1)

(define btree-max
  (lambda (tr)
    (match tr
      ((make-empty-tree) -inf.0)
      ((make-node l val r) (max (btree-max l) val (btree-max r))))))



