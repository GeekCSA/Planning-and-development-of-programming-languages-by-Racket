#lang pl untyped

;; ################################## Question number 1 ##################################


;; The function checks if one of the strings contains the string "pl" as a prefix.
;; 
;; The function runs on each string the "checkStartPL" function that checks
;; if a given string contains the string "pl" as a prefix.
;;
;; Input: 5 strings
;; Output: the first string that starts with "pl", else return #f
;;
;; Example: '("yypl" "opl" "lpTT" "lpl" "lol") => false
;;          '("l" "p" "" "pl" "lol") => "pl"
;;
;; Description of the work process: this function is simple to execute
;;    because it is only a function beam on each of the five strings or false returns

(: plPrefixContained : (String String String String String) -> (U String Boolean))
(define (plPrefixContained s1 s2 s3 s4 s5)
  (cond
    [(checkStartPL s1) s1] ;; Check if s1 contains the string "pl" as a prefix
    [(checkStartPL s2) s2] ;; Check if s2 contains the string "pl" as a prefix
    [(checkStartPL s3) s3] ;; Check if s3 contains the string "pl" as a prefix
    [(checkStartPL s4) s4] ;; Check if s4 contains the string "pl" as a prefix
    [(checkStartPL s5) s5] ;; Check if s5 contains the string "pl" as a prefix
    [else #f])) ;; No found strings that contains the string "pl" as a prefix


;; The function checks if a string contains the string "pl" as a prefix.
;;
;; First of all the function checks if the length of the string is not less than 2,
;; if the length is correct then the function check if the first character in the string is 'p' and the next character is 'l'.
;; If both of the above conditions do not occur then the function will return
;;
;; Input: A string
;; Output: #t if the string contains the string "pl" as a prefix, else return #f
;;
;; Example: "pla12a" => true
;;          "e" => false
;;
;; Description of the work process: this function is very simple
;;    because it does not have many conditions

(: checkStartPL : (String -> Boolean))
(define (checkStartPL s)
  (cond
    [(< (string-length s) 2) #f] ;; Check that |s| < 2
    [(and (if (eq? (string-ref s 0) #\p ) #t #f) (if (eq? (string-ref s 1) #\l ) #t #f)) #t] ;; Check if s[0] == 'p' and s[1] == 'l'
    [else #f] ;; Return #f because either |s| < 2 or  s[0] != 'p' or s[1] != 'l'
    ))

(test (plPrefixContained "yypl" "opl" "lpTT" "lpl" "lol") => false)
(test (plPrefixContained "" "" "" "" "") => false)
(test (plPrefixContained "12" "p1l" "plpl" "" "lplp") => "plpl")
(test (plPrefixContained "pll" "pll" "pll" "pll" "lol") => "pll")
(test (plPrefixContained "l" "p" "" "pl" "lol") => "pl")
