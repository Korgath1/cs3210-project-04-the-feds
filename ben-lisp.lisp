;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1: Member of Set Checker

(defun set-member (set item)
  ;; Base case: empty set means `item` is not in `set`
  (COND ((NULL set) NIL)
        ;; if exists return true
        ;; CAR Returns first element
        ((EQUAL (CAR set) item) T)
        ;; recursively go through set
        ;; Gets the rest of the list excluding the first element
        ((set-member (CDR set) item))
  )
)

;; Test cases
(FORMAT T "Test 1: (set-member '(1 2) 1) => ~a~%" (set-member '(1 2) 1))
(FORMAT T "Test 2: (set-member '(1 2) 3) => ~a~%" (set-member '(1 2) 3))
(FORMAT T "Test 3: (set-member '(a b c) 'b) => ~a~%" (set-member '(a b c) 'b))
(FORMAT T "Test 4: (set-member '() 5) => ~a~%" (set-member '() 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2: Return the union of two sets

;; Local variables:
;; first-elem: the first element of set-1
;; rest-union: the union of the rest of set-1, and set-2

(defun set-union (set-1 set-2)
  ;; Base cases need 2 this time!
  ;; If set-1 is empty, return set-2 If set-2 is empty, return set-1 
  (COND ((NULL set-1) set-2) 
        ((NULL set-2) set-1) 
        ;; add the first element of set-1 to the union of the rest of set-1, and set-2 recursive brain pain
        ;; Only if it's not already in set-2
        (T (LET ((first-elem (CAR set-1))
                 (rest-union (set-union (CDR set-1) set-2)))
           ;; If first element is already in set-2, just return the union of the rest
           (IF (set-member set-2 first-elem)
               rest-union
               ;; Otherwise, add it to the result
               (CONS first-elem rest-union))))))

;; Test cases
(FORMAT T "Test Union 1: (set-union '(1 2) '(2 4)) => ~a~%" (set-union '(1 2) '(2 4)))
(FORMAT T "Test Union 2: (set-union '() '(3 4)) => ~a~%" (set-union '() '(3 4)))
(FORMAT T "Test Union 3: (set-union '(a b) '(c d)) => ~a~%" (set-union '(a b) '(c d)))
(FORMAT T "Test Union 4: (set-union '(1 2 3) '()) => ~a~%" (set-union '(1 2 3) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Problem 3: Return the intersection of two sets
;; Return the intersection of set-1 and set-2.

;; The result should contain no duplicates.

;; Assume set-1 contains no duplicates and set-2 contains no duplicates.

;; Examples:

;;   (set-intersection '(1 2) '(2 4)) => '(2)

(defun set-intersection (set-1 set-2)

  ;;Your implementation go here
  (set-member set-1 set-2)

)

(FORMAT T "~a~%" (set-intersection '(1 2 3) '(2 4)))
