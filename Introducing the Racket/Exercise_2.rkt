#lang pl untyped

;; ################################## Question number 2.1 ##################################


;; The function return the longest string that is an element of this list, and #f if no such string exists.
;;
;; The function calls to helper function that returns the longest string in the list or return some item from the list (not string).
;; The function checks if the variable, that returned from helper function, is string or not.
;;    if the variable is strings then the function returns it else returns false.
;;
;; Input: list of any
;; Output: the longest string that is an element of this list, and #f if no such string exists.
;;
;; Example: '(34 uuu 90)) => false
;;          '(uu 56 oooo "r" "rRR" "TTT")) => "rRR"
;;
;; Algorithm:
;;
;; if(list == null) return false
;; else {
;;    myResult = helper(list.rest, list.first)
;;    if(typeOf(myResult) == string) return myResult
;;    else return false
;; }
;;
;; Description of the work process: In this function, I had a dilemma about what to say in the parameters in the recursive calls,
;;    so it took a little more time to plan it.


(: longestString : Listof Any -> (U String Boolean))
(define (longestString list)
  (if (null? list)
      #f
      (let ([myResult (helper_find_max_str_first (rest list) (first list))]) ;; myResult = output from helper function
        (if (string? myResult) myResult #f))))

;; The function return the longest string that is an element of this list, or some item from the list.
;;
;; The function go through the list and checks each item if it string and check if the len(next_item) bigger than len(max_str),
;;         if len(next_item) > max then max_str = next_item
;;
;; Input: list of any and the variable from type Any
;; Output: the first longest string, if there is not strings in the list, the function returns some item from list.
;;
;; Example: '(43 uuu 90)) => 90
;;          '(uu 56 oooo "r" "rRR" "TTT")) => "rRR"
;;
;; Algorithm:
;;
;; if (list == null) return maxStr
;; else {
;;    item = list.first
;;    if(typeOf(maxStr) == string) {
;;       if(typeOf(item) == string AND len(item)>len(maxStr)) return helper(list.rest, item)
;;       else return helper(list.rest, maxStr)
;;   } else return helper(list.rest, item) 
;;
;; This written for next question also!!
;;
;; Description of the work process: this function had a thought about how I could solve all the cases.

(: helper_find_max_str_first : ((Listof Any) Any) -> String)
(define (helper_find_max_str_first list maxStr)
  (if (null? list)
      maxStr
      (let ([item (first list)])
                 (if (string? maxStr) (if (and
                      (string? item)
                      (> (string-length item) (string-length maxStr)))
                     (helper_find_max_str_first (rest list) item)
                     (helper_find_max_str_first (rest list) maxStr))
                     (helper_find_max_str_first (rest list) item)) 
        )))

(test (longestString '(34 uuu 90)) => false)
(test (longestString '()) => false)
(test (longestString '(34 uuu 90 "")) => "")
(test (longestString '(uu 56 oooo "r" "rRR" "TTT")) => "rRR")


;; ################################## Question number 2.2 ##################################


;; The function returns the shortest and longest strings for each list in the list of lists, and #f if no such list exists.
;;
;; The function calls to helper functions that return the shortest and longest strings in the list or returns empty list (not string).
;;
;; Input: list of list of any
;; Output: the shortest and longest strings for each list in the list of lists, and #f if no such list exists.
;;
;; Example: '((any "Benny" 10 "OP" 8) (any Benny OP (23))) => '(("OP" "Benny") ())
;;          '(("2 5 5" 1 "5gg" L) (v gggg "f") ())) => '(("5gg" "2 5 5") ("f" "f") ())
;;
;; Algorithm:
;;
;; if(list == null) return false
;; else map(lambda (x)-> short&long-one-list(x), list)
;;
;; Description of the work process: this was simple because of the use of the map.

(: short&long-lists : (Listof(Listof Any)) -> (U Boolean (Listof(Listof String))))
(define (short&long-lists list) 
  (if (null? list)
      #f 
      (map (lambda (innerList)
         (short&long-one-list innerList))
       list)))

;; The function return the shortest and longest strings that are elements of this list, and #f if no such string exists.
;;
;; The function calls to helper functions that return the shortest and longest strings in the list or return some item from the list (not string).
;; The function checks if the variables, that returned from helper functions, is string or not.
;;    if the variables are strings then the function returns list of them else returns empty list.
;;
;; Input: list of any
;; Output: the shortest and longest strings that are elements of this list, and #f if no such string exists.
;;
;; Example: '(34 uuu 90)) => '()
;;          '(uu 56 oooo "r" "rRR" "TTT")) => '("r" "rRR")
;;
;; Algorithm:
;;
;; if(list == null) return '()
;; else {
;;    max_str = helper_find_max_str(list.rest, list.first)
;;    min_str = helper_find_min_str(list.rest, list.first)
;;    if(typeOf(max_str) == string AND typeOf(min_str) == string)
;;       return '(valueOf(min_str) valueOf(max_str))
;;    else return '()
;; }
;;
;; Description of the work process: After the auxiliary functions were already built,
;;    the construction of this function was simple
;;    because it was only necessary to call the auxiliary functions.


(: short&long-one-list : (Listof Any) -> (U (Listof Any) Boolean))
(define (short&long-one-list list)
  (if (null? list)
      '()
      (let ([max_str (helper_find_max_str (rest list) (first list))]
            [min_str (helper_find_min_str (rest list) (first list))]) ;; myResult = output from helper function
        (if (and (string? max_str) (string? min_str)) (cons min_str (cons max_str null)) '() ))))


;; The function return the shortest string that is an element of this list, or some item from the list.
;;
;; The function go through the list and checks each item if it string and check if the len(next_item) smaller than len(max_str),
;;         if len(next_item) < min_str then min_str = next_item
;;
;; Input: list of any and variable (Any)
;; Output: the first shortest string, if there is not strings in the list, the function returns some item from list.
;;
;; Example: '(43 uuu 90)) => 90
;;          '(uu 56 oooo "r" "rRR" "TTT")) => "r"
;;
;;Algorithm:
;;
;; if (list == null) return minStr
;; else {
;;    item = list.first
;;    if(typeOf(minStr) == string) {
;;       if(typeOf(item) == string AND len(item)<len(minStr)) return helper(list.rest, item)
;;       else return helper(list.rest, minStr)
;;    } else return helper(list.rest, item)
;;
;; Description of the work process: this function had a thought about how I could solve all the cases.

(: helper_find_min_str : ((Listof Any) Any) -> String)
(define (helper_find_min_str list minStr)
  (if (null? list)
      minStr
      (let ([item (first list)])
                 (if (string? minStr) (if (and
                      (string? item)
                      (< (string-length item) (string-length minStr)))
                     (helper_find_min_str (rest list) item)
                     (helper_find_min_str (rest list) minStr))
                     (helper_find_min_str (rest list) item))
        )))

;; The function return the longest string that is an element of this list, or some item from the list.
;;
;; The function go through the list and checks each item if it string and check if the len(next_item) bigger than len(max_str),
;;         if len(next_item) > max then max_str = next_item
;;
;; Input: list of any and variable (Any)
;; Output: the first longest string, if there is not strings in the list, the function returns some item from list.
;;
;; Example: '(43 uuu 90)) => 90
;;          '(uu 56 oooo "r" "rRR" "TTT")) => "rRR"
;;
;;Algorithm:
;;
;; if (list == null) return maxStr
;; else {
;;    item = list.first
;;    if(typeOf(maxStr) == string) {
;;       if(typeOf(item) == string AND len(item)>len(maxStr)) return helper(list.rest, item)
;;       else return helper(list.rest, maxStr)
;;    else return helper(list.rest, item)
;;
;; Description of the work process: this function had a thought about how I could solve all the cases.

(: helper_find_max_str : ((Listof Any) Any) -> String)
(define (helper_find_max_str list maxStr)
  (if (null? list)
      maxStr
      (let ([item (first list)])
                 (if (string? maxStr) (if (and
                      (string? item)
                      (> (string-length item) (string-length maxStr)))
                     (helper_find_max_str (rest list) item)
                     (helper_find_max_str (rest list) maxStr))
                     (helper_find_max_str (rest list) item))
        )))

(test (short&long-one-list '(34 uuu 90)) => '() )
(test (short&long-one-list '()) => '() )
(test (short&long-one-list '(34 uuu 90 "")) => '("" ""))
(test (short&long-one-list '(uu 56 oooo "r" "rRR" "TTT")) => '("r" "rRR"))

(test (short&long-lists '() ) => #f )
(test (short&long-lists '((any "Benny" 10 "OP" 8) (any Benny OP (23)))) => '(("OP" "Benny") () ))
(test (short&long-lists '(("2 5 5" 1 "5gg" L) (v gggg "f") ())) => '(("5gg" "2 5 5") ("f" "f") () ))

