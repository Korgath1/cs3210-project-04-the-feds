;; Return T if item is a member of set.

;; Return NIL if item is not a member of set.

;; The type of set is list.

;; Examples:

;; (set-member '(1 2) 1) => T

;; (set-member '(1 2) 3) => NIL

(defun set-member (set item)

  ;; Base case: empty set means item is not in set
  (cond ((null set) nil)
        ;; if exists return true
        ;; car returns first element      
        ((equal (car set) item) t)
        ;; recursively go through set
        ;; gets the rest of the list excluding the first element       
        ((set-member (cdr set) item))
   )
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the union of set-1 and set-2.

;; The result should contain no duplicates.

;; Assume set-1 contains no duplicates and set-2 contains no duplicates.

;; Examples:

;; (set-union '(1 2) '(2 4)) => '(1 2 4)

;; Local variables;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the intersection of set-1 and set-2.

;; The result should contain no duplicates.

;; Assume set-1 contains no duplicates and set-2 contains no duplicates.

;; Examples:

;; (set-intersection '(1 2) '(2 4)) => '(2)

(defun set-intersection (set-1 set-2)

  ;;Base case goes here
  ;;(myFun2 set-1 set-2)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the difference of set-1 and set-2.

;; The result should contain no duplicates.

;; Assume set-1 contains no duplicates and set-2 contains no duplicates.

;;

;; Examples:

;; (set-diff '(1 2) '(2 4)) => '(1)

(defun set-diff (set-1 set-2)

  ;;Your implementation go here

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the exclusive or of a and b

;;

;; Examples:

;; (boolean-xor t nil) => t

;; (boolean-xor nil nil) => nil

(defun boolean-xor (a b)

  ;; Your implementation go here
  ;; A     and Bnot
  ;; Anot  and B
  ;; ABnot or AnotB 

  (OR (AND a (NOT b)) (AND (NOT a) b))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the implication of a and b

;;

;; Examples:

;; (boolean-implies t nil) => nil

;; (boolean-implies nil nil) => t

(defun boolean-implies (a b)

  ;;
  (OR (NOT a) b)
  
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the bi-implication (if and only if) of a and b

;;

;; Examples:

;; (boolean-iff t nil) => nil

;; (boolean-iff nil nil) => t

(defun boolean-iff (a b)

  ;; A implies B
  ;; B implies A
  (AND(boolean-implies a b) (boolean-implies b a))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Evaluate a boolean expression.

;; Handle NOT, AND, OR, XOR, IMPLIES, and IFF.

;;

;; Examples:

;; (boolean-eval '(and t nil)) => nil

;;; (boolean-eval '(and t (or nil t)) => t

(defun boolean-eval (exp)

  ;; Motivating idea;
  ;;  CASE I  item is atom
  ;;  CASE II item is list
  ;; NOT AND OR XOR IMPLIES IFF

  ;; dummy code;
  ;; base case, item is atom -> return
  ;; if item is list check contents (recursive)
  ;;
  (COND
    ;; like switch, will execute on the first true instance or skip otherwise
    ;;  check if (first) item is NIL
    ;;  check if (first) item is T
    ;;  check if (first) item is statement
    ;;   NOT AND OR XOR IMPLIES IFF
    
    ;; BASE CASE IS ATOM NIL OR
    ((EQUAL exp T) T)
    ((EQUAL exp NIL) NIL)

    ;; NOT
    ((EQUAL (CAR exp) 'NOT)
     (NOT (boolean-eval (SECOND exp))))

    ;; AND
    ((EQUAL (CAR exp) 'AND)
     (AND (boolean-eval (SECOND exp))
          (boolean-eval (THIRD exp))))
    ;; OR

    ((EQUAL (CAR exp) 'OR)
     (OR (boolean-eval (SECOND exp))
         (boolean-eval (THIRD exp))))
    
    ;; IMPLIES
    
    ((EQUAL (CAR exp) 'IMPLIES)
     (boolean-implies (boolean-eval (SECOND exp))
                      (boolean-eval (THIRD exp))))
    
    ;; IFF

    ((EQUAL (CAR exp) 'IFF)
     (boolean-iff (boolean-eval (SECOND exp))
                  (boolean-eval (THIRD exp))))
    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Perform merge sort on the lists.

;; Parameters:

;; list: The list to sort

;; predicate: A function to compare elements of the list

;;

;; Examples:

;; (merge-sort '(2 1 5 0) #'<) => '(0 1 2 5)

;; (merge-sort '(2 1 5 0) #'>) => '(5 2 1 0)

;;Motivating items
;; BASE CASE
;; Recursion

;; another thing to consider is when we call merge sort but the list is an odd
;; number and can not be split evenly (?)

;; We will need a merge helper function

;; CASE I item is an atom and contains only one item
;; CASE II item is a list, call merge sort again

;; Pseudo-code

;; splitting the list
;; calculate the midpoint of the list
;; left half to midpoint
;; midpoint inclusive to rest of the list

;; merge the two sorted lists
;; compare first elements of both lists
;; add the smallest element 

(defun merge-sort (list predicate))



;; RIGHT NOW THIS ONLY TAKES TWO SORTED SUB-LISTS AND SORTS THEM UP TO THE NEXT LEVEL
;; EX. (merge-up '(1 3 5) '(2 4 6) #'<)
;; EX. (merge-up '(9 6 3) '(8 5 2) #'>)

;; notes for tomorrow, split functionality

(defun merge-up (left right predicate)
  (COND

    ;; IF the left is empty, return the right list
    ;; IF the right is empty, return the left list
    
    ((NULL left) right)
    ((NULL right) left)

    ;; Here we use funcall to pass in the comparison
    
    
    ((FUNCALL predicate (CAR left) (CAR right))
     (CONS (CAR left) (merge-up (CDR left) right predicate)))
    (T
     (CONS (CAR right) (merge-up left (CDR right) predicate)))))











;; SOME TEST FUNCTION
(defun myFun1 (num1 num2)
  (+ num1 num2)
  )

(defun myFun2 (set1 set2)
  (cons set1 set2)
  )

