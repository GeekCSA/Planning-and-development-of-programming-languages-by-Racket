#lang pl untyped

;; ################################## Question number 3 ##################################


;; Implementation of a stack.
;; I implement the below function for a stuck:
;; 1) Push
;; 2) Pop
;; 3) Search
;;
;; I explain each function above its implementation.
;;
;; Each element in the stack builds as a map (key, value), the key is a symbol and the value is a string.
;;
;; I define a type that contains two subtypes EmptyKS (empty stack, constructor) and Push (element in the stack).
;;
;; The EmptyKS doesn't get any variable.
;; The Push gets Symbol, String and KeyStack
;;
;; Description of the work process: on this question it was a bit difficult to understand what tools I should use.
;;    But from the moment I understood the question and the form of the solution it has become simpler

(define-type KeyStack
[EmptyKS]
[Push Symbol String KeyStack])

;; The function search a specific key and returns its value.
;; The function goes through on each "node" (Push) and checks if the key is the desired key,
;; if it is the desired key then the function returns its value,
;; otherwise, the function continues to search until arrives the end of the stack and then returns false.       
;;
;; Input: Push type
;; Output: String, if find the key, otherwise, false
;;
;; Example: 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))) => "AAA"
;;          'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))) => #f
;;
;; Algorithm:
;;
;; Switch(typeOf(push))
;;    case (Push key value next):
;;       if(key == char) return value
;;       else return search-stack(char, next)
;;    default: return false
;;
;; Description of the work process: this function is very simple function.

(: search-stack : Symbol Push -> (U String Boolean))
(define (search-stack char push)
  (cases push
    [(Push key value next) (if (equal? key char) value (search-stack char next))]
    [else #f])
  )

;; The function throws the head of the stack.
;;
;; Input: Push type
;; Output: Push, if the stack is not empty, otherwise, false
;;
;; Example: (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))) => (Push 'b "B" (Push 'a "A" (EmptyKS)))
;;          (EmptyKS) => #f
;;
;; Algorithm:
;;
;; Switch(typeOf(push))
;;    case (Push key value next):
;;       return next
;;    default: return false
;;
;; Description of the work process: this function is very simple function.

(: pop-stack : Push -> (U Push EmptyKS))
(define (pop-stack push)
  (cases push
    [(Push key value next) next]
    [else #f])
  )

(test (EmptyKS) => (EmptyKS))
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))) => (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "AAA")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (pop-stack (EmptyKS)) => #f)
