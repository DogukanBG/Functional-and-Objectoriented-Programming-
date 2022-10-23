;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname H05_Bagci_Dogukan) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; FOP-Hausübung H5
; Bitte denken Sie daran, die Datei vor der Abgabe entsprechend der Namenskonvention umzubenennen!
; Zuweisungen mittels set!, let, begin usw. sind in keiner Racket-Hausübung erlaubt.
; Ebenso darf dynamische Codeausführung, bspw. per eval, in keiner Racket-Hausübung verwendet werden.
; Funktionen höherer Ordnung sind in dieser Hausübung nicht erlaubt.


;
; Structs
;
(define-struct permutation (input output))

;;Test Permutations
(define p1 (make-permutation (list 3 4 5 6) (list 5 6 3 4)))
(define p2 (make-permutation (list 10 2 7 21) (list 2 10 21 7)))
(define p3 (make-permutation (list 2 7 21 10) (list 7 21 10 2)))
(define p4 (make-permutation (list 10 2 7 21) (list 21 2 7 10)))
(define p5 (make-permutation (list 1 3 5 7) (list 5 3 1 7)))
(define p6 (make-permutation (list 3 1 5 7) (list 7 1 5 3)))
(define p7 (make-permutation (list 7 1 3 5) (list 3 7 5 1)))


;
; H1
;

;;Type: (Liste)(Zahl) --> Auskunft, ob Zahl in der Liste vorhanden ist
;;Return -->  Auskunft, ob Zahl in der Liste vorhanden ist
(define (contains-x? lst x)
  (cond
    [(empty? lst) #false]
    [(= (first lst) x) #true]
    [else (contains-x? (rest lst) x)]))

;;Tests:
(check-expect (contains-x? (list 4 5 6) 6) #t)
(check-expect (contains-x? (list 4 5 6) 7) #f)
(check-expect (contains-x? (list 4 19 6) 19) #t)

;; Type: (Liste von Zahlen) (Zahl) ---> (Liste von Zahlen)
;; Returns: Eine Liste ohne das erste Vorkommen der eingegebenen Zahl
(define (lst-without-first-occurrence-of lst element)
  (cond
    [(empty? lst) empty]
    [(= (first lst) element)(rest lst)]
    [else(cons (first lst)(lst-without-first-occurrence-of (rest lst) element))]))     
;; Tests:
(check-expect(lst-without-first-occurrence-of (list 10 4 5 6 22 777 30 4) 4) (list 10 5 6 22 777 30 4))
(check-expect(lst-without-first-occurrence-of empty 200000) empty)
(check-expect(lst-without-first-occurrence-of (list 45 67 100 45 23 76) 123) (list 45 67 100 45 23 76))
    
;; Type: (Liste von Zahlen)(Liste von Zahlen) ---> boolean
;; Returns: true, if output is permutation of input
(define (is-permutation? input output)
  (cond
    [(and (empty? input)(empty? output)) #t]
    [(not (=(length input)(length output))) #f]
    [(contains-x? output (first input)) (is-permutation? (rest input) (lst-without-first-occurrence-of output (first input)))]
    [else #f]))   
;; Tests:
(check-expect (is-permutation? empty empty) #t)
(check-expect (is-permutation? (list 1 2 3 4) (list 4 5 6)) #f)
(check-expect (is-permutation? (list 1 2 3) (list 4 5 6)) #f)
(check-expect (is-permutation? (list 4 3 1 2) (list 2 1 3 4)) #t)


;
; H2
;

;;Type: (list of number)(list of number) empty empty --> (list (list of number))
;;Precondition: rst-ve and rst-ho must be the same
;;Returns: all permutations of the given list
(define (all-perm-help rst-ve rst-ho prefix accu)
  (cond
   [(empty? rst-ho) (append (list prefix) accu)]
   
   [(not(empty? (rest rst-ve)))(append (all-perm-help (rest rst-ve) rst-ho prefix accu)
                                    (all-perm-help (lst-without-first-occurrence-of rst-ho (first rst-ve))
                                           (lst-without-first-occurrence-of rst-ho (first rst-ve))
                                           (cons (first rst-ve) prefix) accu))]
   
   [else (all-perm-help (lst-without-first-occurrence-of rst-ho (first rst-ve))
                (lst-without-first-occurrence-of rst-ho (first rst-ve))
                (cons (first rst-ve) prefix) accu)]))
;;Tests:
(check-expect(all-perm-help empty empty empty empty) (list empty))
(check-expect(all-perm-help (list 1)(list 1) empty empty) (list(list 1)))
(check-expect(all-perm-help (list 100 20) (list 100 20) empty empty) (list (list 100 20)(list 20 100)))
(check-expect(all-perm-help (list 400 11 7)(list 400 11 7) empty empty)(list (list 400 11 7) (list 11 400 7)
                                                                             (list 400 7 11) (list 7 400 11)
                                                                             (list 11 7 400) (list 7 11 400))) 


;; Type: (list of number) --> (list of (list of number))
;; Returns: All possible permutations of this list
(define(all-permutations lst)
  (all-perm-help lst lst empty empty))
;;Tests:
(check-expect (all-permutations empty) (list empty))
(check-expect (all-permutations (list 200000000)) (list (list 200000000)))
(check-expect (all-permutations (list 4 5)) (list (list 4 5)(list 5 4)))
(check-expect (all-permutations (list 10 7 17 9)) (list (list 10 7 17 9)(list 7 10 17 9)(list 10 17 7 9)(list 17 10 7 9)
                                                        (list 7 17 10 9)(list 17 7 10 9)(list 10 7 9 17)(list 7 10 9 17)
                                                        (list 10 9 7 17)(list 9 10 7 17)(list 7 9 10 17)(list 9 7 10 17)
                                                        (list 10 17 9 7)(list 17 10 9 7)(list 10 9 17 7)(list 9 10 17 7)
                                                        (list 17 9 10 7)(list 9 17 10 7)(list 7 17 9 10)(list 17 7 9 10)
                                                        (list 7 9 17 10)(list 9 7 17 10)(list 17 9 7 10)(list 9 17 7 10)))
              
;
; H3
;

;;Type:(list of number)(list of number)(list of number) number --> number
;;Precondition: all elements of out have to be in input list  & out, input and output must be the same length  
;;Returns: functional Value of all elements in out
(define (two-line-lookup out input output )
  (cond
    [(or (empty? out) (empty? input)(empty? output)) empty]
    [(= (first out) (first input))(cons (first output) (two-line-lookup (rest out) (rest input) (rest output)))]
    [else (two-line-lookup out (append (rest input)(cons (first input)empty))(append (rest output)(cons (first output) empty)))]))
;;Tests:
(check-expect (two-line-lookup (list 2 1 4 3)(list 1 2 3 4)(list 3 2 1 4))(list 2 3 4 1))
(check-expect (two-line-lookup (list 7 21 10 2)(list 10 2 7 21)(list 2 10 21 7))(list 21 7 2 10))
(check-expect (two-line-lookup empty empty empty) empty)

                 
;;Type: (permutation)(permutation) --> permutation
;;Returns: Composition of the two given permutations
(define (compose-two-permutations p1 p2)
  (make-permutation (permutation-input p2) (two-line-lookup (permutation-output p2)(permutation-input p1)(permutation-output p1))))
;;Tests:
(check-expect(compose-two-permutations p2 p3)(make-permutation (list 2 7 21 10) (list 21 7 2 10)))
(check-expect(compose-two-permutations p3 p4)(make-permutation (list 10 2 7 21) (list 10 7 21 2)))
(check-expect(compose-two-permutations p2 p4)(make-permutation (list 10 2 7 21) (list 7 10 21 2)))
(check-expect(compose-two-permutations (make-permutation empty empty)(make-permutation empty empty))(make-permutation empty empty))

    
;; Type: (list of permuatation) --> permutation
;;Precondition: permutations is not allowed to be empty
;; Returns: composition of any number of permutations
(define (compose-permutations permutations)
  (cond
    [(= 1 (length permutations))(first permutations)]
    [(= 2 (length permutations))(compose-two-permutations (first permutations)(first (rest permutations)))]
    [else(compose-two-permutations (first permutations) (compose-permutations (rest permutations)))]))
;;Tests:
(check-expect(compose-permutations (list p2 p3 p4))(make-permutation (list 10 2 7 21) (list 2 21 7 10)))
(check-expect(compose-permutations (list p2))p2)
(check-expect (compose-permutations (list p5 p6 p7))(make-permutation (list 7 1 3 5) (list 7 3 1 5)))
(check-expect(compose-permutations (list (make-permutation empty empty)(make-permutation empty empty)(make-permutation empty empty))) (make-permutation empty empty))

;
; H4
;

;;Type: (list of number)(list of number) number --> number
;;Precondition: number has to be in input & input and output need at least one element 
;;Returns: functional value of number
(define (lookup input output x)
  (cond
    [(= x (first input)) (first output)]
    [else (lookup (rest input)(rest output) x)]))
;;Tests:
(check-expect (lookup (list 1 2 3) (list 4 5 6) 2) 5)
(check-expect (lookup (list 1 2 3) (list 4 5 6) 1) 4)
(check-expect (lookup (list 1 2 3) (list 4 5 6) 3) 6)



;;Type: (list of number)(list of number)(number)(number) empty --> (list of number)
;;Precondition: input and output are permutations of each other & x and x2 are the same
;;Returns: the current-cycle 
(define (current-cycle input output x x2 accu)
  (cond
    [(empty? input)empty]
    [(= x2 (lookup input output x)) (cons x accu)]
    [else (current-cycle input output  (lookup input output x) x2 (append accu (list x)))]))
;;Tests
(check-expect(current-cycle (list 1 2 3 5 4)(list 5 3 2 4 1)  3 3 empty)(list 2 3))
(check-expect(current-cycle (list 1 2 3 5 4)(list 5 3 2 4 1) 5 5 empty)(list 1 5 4))
(check-expect(current-cycle empty empty  0  0 empty)empty)

;;Type: (list of (list of number)) --> (list of number)
;;Returns: flattens a nested list
(define (collect lst)
  (cond
    [(empty? lst) empty]
    [(list? (first lst)) (append (collect (first lst)) (collect (rest lst)))] 
    [else (cons (first lst) (collect (rest lst)))]))
;;Tests:
(check-expect (collect (list (list 2 3 4) 2 3 5))(list 2 3 4 2 3 5))
(check-expect (collect (list (list 2 3 4) 2 3 5 '()))(list 2 3 4 2 3 5))
(check-expect (collect (list (list 2 3 4) (list 7 6 2)))(list 2 3 4 7 6 2))

;;Type:(list of (list of number)) number --> boolean
;;Returns: true, if the number is in the list
(define(contains-x-deep? lst x)
  (contains-x? (collect lst)x))
;;Tests:
(check-expect (contains-x-deep? (list (list 2 3 4) 2 3 5) 3) #t)
(check-expect (contains-x-deep? (list (list 2 3 4) 2 3 5) 20) #f)
(check-expect (contains-x-deep? empty 3) #f)
    
    

;;Type: (list of number)(list of number)(list of number) empty --> (list of (list of number))
;;Precondition input and output are Permutations of each other & input and not-visited are the same
;;Returns: Cycle notation of given permutation
(define (decomp3 input output not-visited accu)
  (cond
    [(empty? not-visited)accu]
    [(contains-x-deep? accu (first not-visited))(decomp3 input output (rest not-visited) accu)]
    [else(decomp3 input output (rest not-visited) (cons (current-cycle input output (first not-visited)(first not-visited) empty) accu))]))
;;Tests:
(check-expect (decomp3 (list 1 2 3 5 4)(list 5 3 2 4 1)(list 1 2 3 5 4) empty)(list (list 3 2) (list 4 1 5)))
(check-expect (decomp3 (list 1 2 3 4 5 6 7 8 9)(list 2 3 4 5 6 7 8 9 1)(list 1 2 3 4 5 6 7 8 9) empty)(list (list 9 1 2 3 4 5 6 7 8)))
(check-expect (decomp3 empty empty empty empty)empty)

    


;; Type: (list of number) (list of number) --> (List of (List of number))
;;Precondition: output has to be a permutation of input
;; Returns: Cycle notation of given permutation
(define (decompose-permutation-into-cycles input output)
  (decomp3 input output input empty))  
;; Tests:
(check-expect(decompose-permutation-into-cycles empty empty)empty)
(check-expect(decompose-permutation-into-cycles (list 10 2 4 6 7)(list 2 10 4 6 7))(list (list 7) (list 6) (list 4) (list 2 10)))
(check-expect(decompose-permutation-into-cycles (list 3 2 10 7 5 6 48 302 102 20)(list 3 10 7 48 302 102 6 5 2 20))(list (list 20) (list 302 5) (list 102 2 10 7 48 6) (list 3)))


;
; H5
;

;;Type: (list of number)(empty String) --> String
;;Returns: the list as a String with parenthesis 
(define (list-to-string lst accu)
  (cond
    [(empty? lst)(string-append "(" accu ")")]
    [else (list-to-string (rest lst)(string-append (number->string (first lst)) accu))])) 
;;Tests:
(check-expect(list-to-string (list 1 2 3) "")"(321)")
(check-expect(list-to-string empty "")"()")
(check-expect(list-to-string (list 1 2 3 4 5 6 7 8 9) "")"(987654321)")

;;Type: (list of (list of number))(empty String) --> String
;;Precondition: all elements of lst are lists
;;Returns: a nested listed converted into a String with parenthesis
(define(cycle-string-help lst accu)
  (cond
    [(empty? lst)accu]
    [else(cycle-string-help (rest lst)(string-append (list-to-string (first lst) "") accu))]))

(check-expect(cycle-string-help (decompose-permutation-into-cycles (list 1 2 3)(list 3 2 1)) "")"(13)(2)")
(check-expect(cycle-string-help (list (list 5 6 8) (list 1)) "")"(1)(865)")
(check-expect(cycle-string-help empty "")"")

;; Type: (list of number) (list of number) --> String
;;Precondition output has to be a permutation of input
;; Returns: Cycle notation of given pemrutation as a String
(define (two-line-to-cycle-notation input output)
  (cycle-string-help (decompose-permutation-into-cycles input output)""))
;; Tests:
(check-expect(two-line-to-cycle-notation empty empty)"")
(check-expect(two-line-to-cycle-notation (list 4 5 7 6 3 2 1 8) (list 4 7 6 5 2 1 8 3))"(4)(756)(1238)")
(check-expect(two-line-to-cycle-notation (list 4 3 2 1)(list 4 3 2 1))"(4)(3)(2)(1)")