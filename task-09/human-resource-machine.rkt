;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname human-resource-machine) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
; --------------------------------------------------------------------------------------------------------------
; The office
; --------------------------------------------------------------------------------------------------------------

; A post office consists of
; - an inbox for incomming packages
; - an outbox for outgoing packages
; - a floor of fixed size to work on. Each slot contains either a package or nothing
; - a worker who can carry a single package (or nothing)
; - a list of instructions, telling the worker what to do step by step
; - an instruction pointer, telling the worker what to do next. It is either an absolute
;   position on the instructions board, starting from 0, or #f for "finish work!"
; - a time clock which records the total time of work (in number of performed instructions)
(define-record-procedures office
  make-office
  office?
  (inbox
   outbox
   floor-slots
   worker
   instruction-list
   ip
   time-clock))

(: make-office ((list-of package)
                (list-of package)
                (list-of (maybe-of package))
                (maybe-of package)
                (list-of (mixed instruction string))
                (maybe-of natural)
                natural
                -> office))
(: office? (any -> boolean))
(: inbox (office -> (list-of package)))
(: outbox (office -> (list-of package)))
(: floor-slots (office -> (list-of (maybe-of package))))
(: worker (office -> (maybe-of package)))
(: instruction-list (office -> (list-of (mixed instruction string))))
(: ip (office -> (maybe-of natural)))
(: time-clock (office -> natural))

; A package contains either a number or a character
(define package (signature (mixed integer character)))

; A character is an upper case letter of the alphabet between "A" and "Z" 
(define character (signature (predicate (lambda (c) (and (string? c) (string<=? "A" c "Z"))))))

; A (maybe-of ⟨t⟩) is either an element of signature ⟨t⟩ or empty (#f)
(: maybe-of (signature -> signature))
(define maybe-of
  (lambda (t)
    (signature (mixed t (one-of #f)))))


; --------------------------------------------------------------------------------------------------------------
; Some predefined list functions
; --------------------------------------------------------------------------------------------------------------

; replicate an element n times
(: replicate (natural %a -> (list-of %a)))
(define replicate
  (lambda (n x)
    (cond ((= n 0) empty)
          (else (make-pair x (replicate (- n 1) x))))))

; Zip two lists with a given function
(: zipWith ((%a %b -> %c) (list-of %a) (list-of %b) -> (list-of %c)))
(define zipWith
  (lambda (f xs ys)
    (cond ((empty? xs) empty)
          ((empty? ys) empty)
          (else        (make-pair (f (first xs) (first ys)) 
                                  (zipWith f (rest xs) (rest ys)))))))

; Return an integer list range
(: range (integer integer -> (list-of integer)))
(define range
  (lambda (from to)
    (cond ((> from to) empty)
          (else (make-pair from (range (+ from 1) to))))))


; Exercises (f) through (k): implement the higher-order procedures for list processing

;----------------
; F
; Gives for given element and a list the postition in a list
; Apply function (%a -> boolean) on all elements of the list (list-of %a) and gives the number
;----------------
(: list-index ((%a -> boolean) (list-of %a) -> natural))
(check-expect (list-index number? (list "a" 5 "b" "c" "d" "e" )) 1)
(define list-index
  (lambda (funk l1)
    (list-index-worker funk l1 0)))

(: list-index-worker ((%a -> boolean) (list-of %a) natural -> natural))
(check-expect (list-index-worker number? (list "a" 5 "b" "c" "d" "e" ) 0) 1)
(check-expect (list-index-worker number? (list "a" "b" "c" "d" "e" 5) 0) 5)
;(check-expect (list-index-worker true? (list "a" 5 "b" "c" "d" "e" ) 0) (violation "Liste ist leer"))
(define list-index-worker
  (lambda (ent? l1 acc)
    (match l1
      (empty (violation "Liste ist leer"))
      ((make-pair val rst) (if (ent? val) acc
                               (list-index-worker ent? rst (+ acc 1)))))))


      
; --------------------------------------------------------------------------------------------------------------
; Instructions
; --------------------------------------------------------------------------------------------------------------

; An instruction consists of
; - a text representation and
; - a function that modifies a given office, following the instruction
(define-record-procedures instruction
  make-instr
  instr?
  (description action))

(: make-instr (string (office -> office) -> instruction))
(: instr? (any -> boolean))
(: description (instruction -> string))
(: action (instruction -> (office -> office)))





; Exercises (a), (b), (g), (i), (k), (l) and (n): implement the instructions

;------------
; A
; Worker gets an element out of inbox
;------------
(: <-inbox instruction)
(define <-inbox
  (make-instr "<-inbox" (lambda (o) (match o
                                      ((make-office in out flo w l1 ip t) (if (empty? in) (make-office in out flo w l1 #f t) (make-office (rest in) out flo (first in) l1 ip t)))))))
;------------
; B
; Worker puts an element into outbox and is empty afterwards
;------------
(: ->outbox instruction)
(define ->outbox
  (make-instr "->outbox" (lambda (o) (match o
                                       ((make-office in out  flo w l1 ip t)(if (empty? w) (violation "Worker has nothing to do") (make-office in (make-pair w out) flo empty l1 ip t)))))))

;------------------
; G
; Lets the worker jump to the next instruction
; BUG equal? is not working with dMdA
;------------------
(: jump (string -> instruction))
(define jump
  (lambda (lbl)
    (make-instr (string-append "jump " lbl)
                (lambda (ofc)
                  (match ofc
                    ((make-office in out flo w l1 ip t) (make-office in
                                                                     out
                                                                     flo
                                                                     w
                                                                     l1
                                                                     (+ 1 (list-index (lambda (elem)
                                                                                        (if (string? elem)
                                                                                            (if (string=? elem lbl) #t
                                                                                                #f)
                                                                                            #f)) l1))
                                                                     t)))))))

;------------
; I
; Jumps to given label if packet of worker is 0
;------------

(: jump-if-zero (string -> instruction))
(define jump-if-zero
  (lambda (lbl)
    (make-instr (string-append "jiz " lbl)
                (lambda (ofc)
                  (match ofc
                    ((make-office in out flo w l1 ip t) (if (= w 0) (make-office in out flo w l1 (+ 1 (list-index (lambda (elem) (string=? elem lbl)) l1)) t)
                                                            ofc)))))))

(: jump-if-negative (string -> instruction))
(define jump-if-negative
  (lambda (lbl)
    (make-instr (string-append "jin " lbl)
                (lambda (ofc)
                  (match ofc
                    ((make-office in out flo w l1 ip t) (if (and (number? w) (negative? w)) (make-office in out flo w l1 (+ 1 (list-index (lambda (elem) (if (string? elem)(string=? elem lbl) #f)) l1)) t)
                                                            ofc)))))))

;------------------
; K
; list-update changes an element on a given position
;------------------
(: list-update ((list-of %a) natural %a -> (list-of %a)))
(define list-update
  (lambda (l1 pos elem)
    (list-update-worker l1 empty pos elem 0)))

(: list-update-worker ((list-of %a) (list-of %a) natural %a natural -> (list-of %a)))
(check-expect (list-update-worker (list 1 3 4 5 6 7 8 9) empty 1 2 0) (list 1 2 4 5 6 7 8 9))
(define list-update-worker
  (lambda (lin lout pos elem acc)
    (match lin
      (empty lout)
      ((make-pair a b) (if (= acc pos)
                           (list-update-worker b (append lout (list elem)) pos elem (+ acc 1))
                           (list-update-worker b (append lout (list a)) pos elem (+ acc 1)))))))

;----------------
; K
; copy-to put a copy of a package on the floor
;----------------
(: copy-to (natural -> instruction))
(define copy-to
  (lambda (x)
    (make-instr (string-append "copy-to " (number->string x))
              (lambda (o)
                (match (worker o)
                  (empty (violation "Worker hat kein Paket"))
                  (_ (match o
                       ((make-office in out flo w1 l1 ip t) (make-office in out (list-update flo x w1) w1 l1 ip t)))))))))

;----------------
; L
; copy-from copies an elemen from floor to worker
;----------------
(: copy-from (natural -> instruction))
(define copy-from
  (lambda (x)
    (make-instr (string-append "copy-from " (number->string x))
              (lambda (o)
                (match (list-ref (floor-slots o) x)
                  (#f (violation "Floor hat kein Paket"))
                  (_ (match o
                       ((make-office in out flo w1 l1 ip t) (make-office in out flo (list-ref (floor-slots o) x) l1 ip t)))))))))



;----------------
; N
; Ordinal gives an positon of char on alphabet
;----------------
(define alphabet (list "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))

(: ordinal (character -> natural))
(check-expect (ordinal "A") 1)
(define ordinal
  (lambda (c)
    (if (number? (string->number c)) (string->number c)
    (+ (list-index (lambda (x) (string=? x c)) alphabet) 1))))

(: sub (natural -> instruction))
(define sub
  (lambda (x)
    (make-instr (string-append "sub " (number->string x))
                (lambda (o)
                  (match (floor-slots o)
                    (empty (violation "Nothing to subtract"))
                    (_ (match o
                         ((make-office in out flo w l1 ip t) (cond ((or (empty? w) (false? (list-ref flo x)))(violation "Floor oder Worker ist leer"))
                                                                   (else (make-office in out flo (- (ordinal (if  (string? w) w (number->string w))) (ordinal (if (string? (list-ref flo x)) (list-ref flo x) (number->string (list-ref flo x))))) l1 ip t)))))))))))

(: add (natural -> instruction))
(define add
  (lambda (x)
    (make-instr (string-append "add " (number->string x))
                (lambda (o)
                  (match (floor-slots o)
                    (empty (violation "Nothing to add"))
                    (_ (match o
                         ((make-office in out flo w l1 ip t) (cond ((or (empty? w) (false? (list-ref flo x)))(violation "Floor oder Worker ist leer"))
                                                                   (else (make-office in out flo (+ (ordinal (if  (string? w) w (number->string w))) (ordinal (if (string? (list-ref flo x)) (list-ref flo x) (number->string (list-ref flo x))))) l1 ip t)))))))))))

(: bump+ (natural -> instruction))
(define bump+
  (lambda (x)
    (make-instr (string-append "bump+ " (number->string x))
                (lambda (o)
                  (match (list-ref (floor-slots o) x)
                    (#f (violation "Nothing to bump"))
                    (_ (match o
                         ((make-office in out flo w l1 ip t) ((action (copy-from x)) (make-office in out (list-update flo x (+ 1 (ordinal (if (string? (list-ref flo x)) (list-ref flo x) (number->string (list-ref flo x)))))) w l1 ip t))))))))))

(: bump- (natural -> instruction))
(define bump-
  (lambda (x)
    (make-instr (string-append "bump- " (number->string x))
                (lambda (o)
                  (match (list-ref (floor-slots o) x)
                    (#f (violation "Nothing to bump"))
                    (_ (match o
                         ((make-office in out flo w l1 ip t) ((action (copy-from x)) (make-office in out (list-update flo x (- (ordinal (if (string? (list-ref flo x)) (list-ref flo x) (number->string (list-ref flo x)))) 1)) w l1 ip t))))))))))
;
; --------------------------------------------------------------------------------------------------------------
; Running the office
; --------------------------------------------------------------------------------------------------------------

;-----------
; C
; Perform the action of the next instruction
;-----------

(: perform-next (office -> office))
(define perform-next
  (lambda (o)
    (match o
      ((make-office in out flo w l1 ip t)
       (cond
         ((false? ip) (make-office in out flo w l1 ip t))
         ((= ip (length l1)) (make-office in out flo w l1 #f t)) 
         ((string? (list-ref l1 ip)) (make-office in out flo w l1 (+ ip 1) t))
         (else ((action (list-ref l1 ip)) (make-office in out flo w l1 (+ ip 1) (+ t 1)))))))))
;------------
; D
; Iteratively apply instructions to a given office
;------------

(: perform-all (office -> office))
(define perform-all
  (lambda (o)
    (match o
      ((make-office in out flo w l1 ip t) (if (not (false? ip)) (perform-all (perform-next o)) o))); Student task: exercise (d)
    ))


; --------------------------------------------------------------------------------------------------------------
; Draw and animate the office
; --------------------------------------------------------------------------------------------------------------

; Draw package
(: draw-package ((maybe-of package) -> image))
(define draw-package
  (lambda (p)
    (place-image (text (cond ((number? p) (number->string p))
                             ((string? p) p)
                             (else ""))
                       14 "black")
                 12 12
                 (overlay
                  (cond ((boolean? p) empty-image)
                        (else (rectangle 20 20 "solid" "lightgray")))
                  (rectangle 23 23 "solid" "brown")
                  (rectangle 24 24 "solid" "white")))))

; Draw list of packages
(: draw-pkgs (string (list-of (maybe-of package)) -> image))
(define draw-pkgs
  (lambda (lbl ps)
    (beside (place-image/align (text lbl 14 "black") (* 2.5 24) 12 "right" "center" (rectangle (* 2.5 24) 24 "solid" "white"))
            (empty-scene 3 0)
            (fold empty-image beside (map draw-package ps)))))

; Draw instruction based on instruction pointer and a given line number
(: draw-instruction ((maybe-of natural) -> (natural (mixed instruction string) -> image)))
(define draw-instruction
  (lambda (ip)
    (lambda (n instr)
      (let ((current? (and (number? ip) (= ip n))))
        (text/font (string-append
                    (if current? ">" " ")
                    (if (< n 10) "0" "")
                    (number->string n) ": "
                    (cond ((string? instr)
                           (string-append "\"" instr "\""))
                          (else (description instr))))
                   16 "black" #f "modern" "normal"
                   (if current? "bold" "normal")
                   #f)))))

; Draw list of instructions
(: draw-instructions ((list-of (mixed instruction string)) (maybe-of natural) -> image))
(define draw-instructions
  (lambda (is ip)
    (above/align "left"
                 (text "Board of instructions: (press any key to proceed, ESC to finish work)"  14 "black")
                 (empty-scene 0 6)
                 (beside (empty-scene 12 0)
                         (fold empty-image
                               (lambda (instr res)
                                 (above/align "left" instr res))
                               (zipWith (draw-instruction ip)
                                        (range 0 (- (length is) 1))
                                        is)))            
                 (empty-scene 0 6))))

; Draw the office
(: draw-office (office -> image))
(define draw-office
  (lambda (o)
    (above/align "left"
                 (text "Human Resource Machine Post Office" 30 "gray")
                 (empty-scene 0 6)
                 (draw-instructions (instruction-list o) (ip o))
                 (empty-scene 0 6)
                 (draw-pkgs "inbox <-" (inbox o))
                 (empty-scene 0 6)
                 (beside (draw-pkgs "worker" (list (worker o)))
                         (draw-pkgs "floor" (floor-slots o)))
                 (empty-scene 0 6)
                 (draw-pkgs "outbox ->" (outbox o))
                 (empty-scene 0 6)
                 (text (string-append "Time clock: " (number->string (time-clock o))) 14 "black")
                 )))

; Animate the office
(: start-office-day (office -> office))
(define start-office-day
  (lambda (o)
    (big-bang o
      (to-draw draw-office)
      (on-key (lambda (o key)
                (cond ((key=? key "escape") (perform-all o))
                      (else (perform-next o))))))))


; --------------------------------------------------------------------------------------------------------------
; Programs and tests
; --------------------------------------------------------------------------------------------------------------

; Solution day 1
;(check-expect (outbox (start-office-day day01)) (list "E" 3 "41" "A" "1"))


(define day01
  (make-office (list 2) empty
               ; inbox , outbox
               (replicate 16 #f) #f ; floor , worker
               (list <-inbox
                     (copy-to 0); instructions:
                     ->outbox
                     (copy-from 0)
                     <-inbox
                     ->outbox)
               0 0)) ; ip , time




(define day02
  (make-office (list "E" 3 41 -10 1) empty    ; inbox, outbox
               (replicate 16 #f) #f  ; floor, worker
               (list "marke1"
                     <-inbox
                     ; instructions:
                     ->outbox
                     (jump "marke1"))            
               0 0))                 ; ip, time

(define day03
  (make-office (list "E" 3 41 -10 1) empty    ; inbox, outbox
               (replicate 16 #f) #f  ; floor, worker
               (list "marke1"
                     <-inbox
                     (jump-if-negative "marke1"); instructions:
                     ->outbox
                     (jump "marke1"))            
               0 0))                 ; ip, time

(define day04
  (make-office (list "E" 10 "A" 5) empty
               ; inbox , outbox
               (replicate 16 #f) #f ; floor , worker
               (list "marke1"
                     <-inbox
                     (copy-to 0); instructions:
                     <-inbox
                     (add 0)
                     (bump- 0)
                     ->outbox
                     (copy-from 0)
                     ->outbox
                     (jump "marke1"))
               0 0)) ; ip , time

(define day05
  (make-office (list 0 20 5) empty
               ; inbox , outbox
               (replicate 16 #f) #f ; floor , worker
               (list <-inbox
                     (copy-to 4)
                     (jump "m1")
                     "m2"
                     (copy-from 2); instructions:
                     ->outbox
                     "m1"
                     <-inbox
                     (copy-to 0)
                     <-inbox
                     (copy-to 1)
                     (copy-from 4)
                     (copy-to 2)
                     "m3"
                     (copy-from 0)
                     (sub 1)
                     (jump-if-negative "m2")
                     (copy-to 0)
                     (bump+ 2)
                     (jump "m3")
                     )
               0 0)) ; ip , time


;(check-expect (outbox (perform-all day01)) (list 3 "E"))
(start-office-day day01)


; Exercises (h), (j), (m) and (o): implement and test the worker's instructions


