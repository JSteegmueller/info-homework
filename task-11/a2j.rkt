;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname a2j) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Vorgegebene Definitionen: Blatt 11 vom 18.1.19
; Aufgabe 2: Parser


; Ein Knoten (node) besteht aus
; - einem linken Zweig (left-branch)
; - einer Markierung (label) und
; - einem rechten Zweig (right-branch)
(: make-node (%a %b %c -> (node-of %a %b %c)))
(: node-left-branch ((node-of %a %b %c) -> %a))
(: node-label ((node-of %a %b %c) -> %a))
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


; Filtert eine Liste gegeben entsprechendem Prädikat
(: filter ((%a -> %b) (list %a) -> (list %b)))

(define filter
  (lambda (p xs)
    (match xs
      (empty empty)
      ((make-pair hd tl)
       (if (p hd)
           (make-pair hd (filter p tl))
           (filter p tl))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
; Eigene Lösung ab hier

; Diese Funktion erstellt aus einem String einen Baum
(: btree-parse (string -> (btree-of string)))
(check-expect (btree-parse "_") the-empty-tree)
(check-expect (btree-parse "(_1_)") (make-node the-empty-tree "1" the-empty-tree))
(check-expect (btree-parse "((_2_)3(_1_))") (make-node (make-node the-empty-tree "2" the-empty-tree) "3" (make-node the-empty-tree "1" the-empty-tree)))

(define btree-parse
  (lambda (s)
    (parser-output (parse (string->strings-list s)))))


; Record für ein parser-tuple
(: make-parser ((list-of string) (btree-of string) -> (parser-of (list-of string) (btree-of string))))
(: parser? (any -> boolean))
(: parser-input (parser -> (list-of string)))
(: parser-output (parser -> (btree-of string)))
(define-record-procedures-parametric parser parser-of
  make-parser
  parser?
  (parser-input
   parser-output))


; Diese Funktion parst genau ein Element einer Liste aus Strings auf einmal und Wertet das Ergebniss in ein parser-Record aus
(: parse ((list-of string) -> (parser-of (list-of string) (btree-of string))))
(check-expect (parse (list "(" "(" "_" "2" "_" ")" "3" "(" "_" "1" "_" ")" ")"))
              (make-parser empty (make-node (make-node the-empty-tree "2" the-empty-tree) "3" (make-node the-empty-tree "1" the-empty-tree))))
(check-expect (parse (list "_"))
              (make-parser empty the-empty-tree))


(define parse
  (lambda (cs)
    (let ((c (first cs)))
      (match c
        ("_" (make-parser (rest cs) the-empty-tree))
        ("(" (letrec ((l (parse (rest cs)))
                      (x (first (parser-input l)))
                      (r (parse (rest (parser-input l)))))
               (make-parser (rest (parser-input r)) (make-node (parser-output l) x (parser-output r)))))
        (wrong (violation (string-append "Fehler in der Eingabe: " wrong))))))) 
                      
  


