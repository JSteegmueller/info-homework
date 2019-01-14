;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname streamsJ) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ())))
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

; 1
; (a) Diese Prozedur akzeptiert einen beliebigen Wert und erstellt daraus einen konstanten Stream
(: const-stream (%a -> (stream-of %a)))
(check-expect (stream-take 5 (const-stream 1)) (list 1 1 1 1 1))
(check-expect (stream-take 5 (const-stream 2)) (list 2 2 2 2 2))

(define const-stream
  (lambda (elem)
    (make-cons elem (lambda () (const-stream elem)))))

; (b) Diese Prozedur erstellt einen Stream f(s) aus einem Stream s und einer Funktion f
(: stream-map ((%a -> %b) (stream-of %a) -> (stream-of %b)))
(check-expect (stream-take 5 (stream-map (lambda (x) (+ x 2)) (const-stream 5))) (list 7 7 7 7 7))
(check-expect (stream-take 5 (stream-map (lambda (x) x) (from 1))) (list 1 2 3 4 5))

(define stream-map
  (lambda (proc stream)
    (make-cons (proc (head stream)) (lambda () (stream-map proc (force (tail stream)))))))

; 2
; (a) Diese Prozedur erzeugt einen Stream nach der vorschrift proc und dem start x
(: stream-iterate ((%a -> %a) %a -> (stream-of %a)))
(check-expect (stream-take 5 (stream-iterate (lambda (x) x) 5)) (list 5 5 5 5 5))
(check-expect (stream-take 5 (stream-iterate (lambda (x) (* x x)) 2)) (list 2 4 16 256 65536))

(define stream-iterate
  (lambda (proc x)
    (make-cons x (lambda () (stream-iterate proc (proc x))))))

; (b) Diese Prozedur gibt das erste Elemente eines konvergierenden Streams aus dessen vorheriges Element sich weniger als d des unterscheidet
(: stream-converge (real (stream-of real) -> real))
(check-within (stream-converge 0.3 (stream-iterate (lambda (x) (/ x 10))
                                                   100))
              0.01
              0.00001)
(check-within (stream-converge 0.001 (stream-iterate (lambda (x) (/ x 2))
                                                   1))
              0.001
              0.0001)

(define stream-converge
  (lambda (d stream)
    (let ((firstNum  (head stream))
          (secondNum (head (force (tail stream)))))
      (if (> d (- firstNum secondNum)) secondNum
          (stream-converge d (force (tail stream)))))))

; (c) Diese Prodezur approximiert eine Quadratwurzel bis sich ein Wert weniger als delta seines Vorgängers unterscheidet
(: approx-sqrt (real real -> real))
(check-within (approx-sqrt 15 0.01) 3.872 0.001)
(check-within (approx-sqrt 4  0.01) 2 0.001)

(define approx-sqrt
  (lambda (a delta)
    (stream-converge delta (stream-iterate (lambda (x)
                                             (/ (+ x (/ a x)) 2)) (/ (+ a 1) 2)))))

; 3 Diese Prozedur berechnet die Potenzmenge einer Menge M
(: powerset ((list-of %a) -> (list-of (list-of %a))))
(check-expect (powerset (list 1 2 3)) (list (list 1 2 3) (list 1 2) (list 2 3) (list 1 3) (list 1) (list 2) (list 3) empty))
(check-expect (powerset (list 1)) (list (list 1) empty))
(check-expect (powerset (list empty)) (list (list empty) empty))

(define powerset
  (lambda (ls)
    (match ls
      (empty (list empty))
      ((make-pair x xs) (append (powerset xs) (map (lambda (j)
                                                     (append (list x) j)) (powerset xs)))))))


                               
                               
                               




