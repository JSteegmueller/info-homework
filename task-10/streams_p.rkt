;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname streams_p) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ())))
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

;----------
; Aufgabe 1
;----------

;A
(: const-stream (%a -> (stream-of %a)))
(define const-stream
  (lambda (num)
    (make-cons num (lambda () (const-stream num)))))

(define ones (const-stream 1))

;B
(: stream-map ((%a -> %b) (stream-of %a) -> (stream-of %b)))
(check-expect (stream-take 5 (stream-map (lambda (x) (+ x 1)) ones)) (list 2 2 2 2 2))
(check-expect (stream-take 5 (stream-map (lambda (x) x) (from 1))) (list 1 2 3 4 5))
(define stream-map
  (lambda (f str) ; function and stream
    (make-cons (f (head str)) (lambda () (stream-map f (force (tail str)))))))

;----------
;Aufgabe 2
;----------

;----------
;A
; stream-iterate erzeugt einen strom der mit c beginnt und dessen weitere Elemente
; die Ergebnisse der wiederholten Anwendung von f
;----------

(: stream-iterate (( %a -> %a) %a -> (stream-of %a)))
(check-expect (stream-take 5 (stream-iterate (lambda (x) (+ x 2)) 1)) (list 1 3 5 7 9))
(define stream-iterate
  (lambda (f c) ; function and starting point
    (make-cons c (lambda () (stream-iterate f (f c))))))

;B
(: stream-converge (real (stream-of real) -> real))
(check-within (stream-converge 0.3 (stream-iterate (lambda (x) (/ x 10)) 100)) 0.01 0.00001)
(define stream-converge
  (lambda (num str)
    (let ((n1 (head str))
         (n2 (head (force (tail str)) )))
    (if (< (- n1 n2) num) n2 (stream-converge num (force (tail str)))))))

;C
(: approx-sqrt (real real -> real))
(check-within (approx-sqrt 15 0.01) 3.872 0.001)
(define approx-sqrt
  (lambda (zahl gen)
    (stream-converge gen (stream-iterate (lambda (x) (/ (+ x (/ zahl x)) 2)) (/ (+ zahl 1) 2)))))

;----------
; Aufgabe 3
;----------

;(: powerset ((list-of %a) -> (list-of (list-of %a))))
(define powerset
  (lambda (xs)
    (powerset-worker xs empty)))

(define powerset-worker
  (lambda (xs acc)
    (match xs
      (empty acc)
      ((make-apir a b) (powerset-worker b (append acc xs))))))