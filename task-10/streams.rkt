;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname streams) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ())))
; ----------------------------------------------------------------------
; Streams (unendliche Ströme von Elementen gleicher Signatur)

; "Versprechen", ein Wert der Signatur t liefern zu können
(define promise
  (lambda (t) 
    (signature (-> t))))

; "Einlösung" (Auswertung) des Versprechens p
(: force ((promise %a) -> %a))
(define force
  (lambda (p) 
    (p)))

; Polymorphe Paare (isomorph zu `pair')
(: make-cons (%a %b -> (cons-of %a %b)))
(: head ((cons-of %a %b) -> %a))
(: tail ((cons-of %a %b) -> %b))
(define-record-procedures-parametric cons cons-of
  make-cons 
  cons?
  (head
   tail))

; Streams mit Elementen der Signatur t
(define stream-of
  (lambda (t) 
    (signature (cons-of t (promise cons)))))

(: from (number -> (stream-of number)))
(define from
  (lambda (n) 
    (make-cons n (lambda () (from (+ n 1))))))

; Erzeuge die ersten n Elemente des Strom str (Stream -> Liste)
(: stream-take (natural (stream-of %a) -> (list-of %a)))
(check-expect (stream-take 5 (from 1)) (list 1 2 3 4 5)) 
(check-expect (stream-take 0 (from 1)) empty)
(define stream-take
  (lambda (n str)
    (match str
      ((make-cons hd tl)
       (if (= n 0)
           empty
           (make-pair hd (stream-take (- n 1) (force tl))))))))


;;; EIGENE LOESUNG AB HIER

