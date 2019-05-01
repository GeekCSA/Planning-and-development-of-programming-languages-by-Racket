#lang pl

;; ################################## Question number 4 ##################################


;; The function checks if a the given number is odd and return true if it odd.
;;
;; Input: A natural number
;; Output: True if the number is odd otherwise, (0 or even) false
;;
;; Algorithm:
;; 
;; if(x == 0) return false
;; else return is-even(x-1) ;; if x-1 is even then x is odd

(: is-odd? : Natural -> Boolean)
(define (is-odd? x)
  (if (zero? x)
      false
      (is-even? (- x 1))))

;; The function checks if a the given number is even and return true if it even.
;;
;; Input: A natural number
;; Output: True if the number is even or 0 otherwise, (odd) false
;;
;; Algorithm:
;; 
;; if(x == 0) return true
;; else return is-even(x-1) ;; if x-1 is even then x is odd

(: is-even? : Natural -> Boolean)
(define (is-even? x)
  (if (zero? x)
      true
      (is-odd? (- x 1))))
;; tests --- is-odd?/is-even?

(test (not (is-odd? 12)))
(test (is-even? 12))
(test (not (is-odd? 0)))
(test (is-even? 0))
(test (is-odd? 1))
(test (not (is-even? 1)))

;; The function calls the function on each element in the list. 
;; The element that given to the function must be from same type of the elements in the list.
;; The given function must return boolean.
;;
;; Input: Function and list
;; Output: True, if all elements in the list returns true by the given function or the list is empty otherwise, false.
;;
;; Algorithm:
;; 
;; if(lst == null OR (pred(list.first) AND every(list.rest))) return true
;; else return false

(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
(define (every? pred lst)
  (or (null? lst)
      (and (pred (first lst))
           (every? pred (rest lst)))))


;; The function calls the "is-even?" function on each element in the list. 
;; The function checks if all the elements in the list 
;;
;; Input: Function and list
;; Output: True, if all elements in the list are even or the list is empty otherwise, false.
;;
;; Algorithm:
;; 
;; if(lst == null OR (all-even?(list.first) AND every(pred, list.rest))) return true
;; else return false

(: all-even? : (Listof Natural) -> Boolean)
(define (all-even? lst)
  (every? is-even? lst))
;; tests
(test (all-even? null))
(test (all-even? (list 0)))
(test (all-even? (list 2 4 6 8)))
(test (not (all-even? (list 1 3 5 7))))
(test (not (all-even? (list 1))))
(test (not (all-even? (list 2 4 1 6))))

;; The function gets two function anf two lists, the first function gets type A as first list.
;; The second function gets type B as the second list.
;; The main function calls on each element in the first list the first function same as with second list and function. 
;; The given functions must return boolean.
;;
;; Input: Two functions and two lists
;; Output: True, if all elements in the lists return true by the given functions or the list is empty otherwise, false.
;;
;; Algorithm:
;; 
;; if(lst == null OR
;;    (pred1(list.first) AND
;;     pred1(list.first) AND
;;     every2(pred1, pred2, list1.rest, list2.rest))) return true
;; else return false

(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) -> Boolean))
(define (every2? pred1 pred2 lst1 lst2)
  (or (null? lst1) ;; both lists assumed to be of same length
      (and (pred1 (first lst1))
           (pred2 (first lst2))
           (every2? pred1 pred2 (rest lst1) (rest lst2)))))
