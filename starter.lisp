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

;; Set Member Tests
(FORMAT T "~%~%~60,,,'-A~%" "-")
(FORMAT T "~15A~A~%" "" "SET MEMBER TESTS")
(FORMAT T "~60,,,'-A~%~%" "-")

(FORMAT T "~2A~A~%" "" "MEMBERSHIP TESTS:")
(FORMAT T "~4ATest 1: (set-member '(1 2) 1) => ~a~%" "" (set-member '(1 2) 1))
(FORMAT T "~4ATest 2: (set-member '(1 2) 3) => ~a~%" "" (set-member '(1 2) 3))
(FORMAT T "~4ATest 3: (set-member '(a b c) 'b) => ~a~%" "" (set-member '(a b c) 'b))
(FORMAT T "~4ATest 4: (set-member '() 5) => ~a~%" "" (set-member '() 5))

(FORMAT T "~%~60,,,'-A~%~%" "-")

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

;; Set Union Tests
(FORMAT T "~%~%~60,,,'-A~%" "-")
(FORMAT T "~15A~A~%" "" "SET UNION TESTS")
(FORMAT T "~60,,,'-A~%~%" "-")

(FORMAT T "~2A~A~%" "" "UNION OPERATIONS:")
(FORMAT T "~4ATest 1: (set-union '(1 2) '(2 4)) => ~a~%" "" (set-union '(1 2) '(2 4)))
(FORMAT T "~4ATest 2: (set-union '() '(3 4)) => ~a~%" "" (set-union '() '(3 4)))
(FORMAT T "~4ATest 3: (set-union '(a b) '(c d)) => ~a~%" "" (set-union '(a b) '(c d)))
(FORMAT T "~4ATest 4: (set-union '(1 2 3) '()) => ~a~%" "" (set-union '(1 2 3) '()))

(FORMAT T "~%~60,,,'-A~%~%" "-")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the intersection of set-1 and set-2.

;; The result should contain no duplicates.

;; Assume set-1 contains no duplicates and set-2 contains no duplicates.

;; Examples:

;; (set-intersection '(1 2) '(2 4)) => '(2)

(defun set-intersection (set-1 set-2)

  ;;  base case check for empty set
  (COND ((NULL set-1) NIL) ;; If set-1 is empty, intersection is empty
        ((NULL set-2) NIL) ;; If set-2 is empty, intersection is empty
        ;;  Get the first element of set-1
        (T (LET ((first-elem (CAR set-1))
                 (rest-inter (set-intersection (CDR set-1) set-2)))
             ;; Check if the first element is in set-2
             ;; Use set-member for recursion and abstractions.
             (IF (set-member set-2 first-elem)
                 ;; If first-elem is a member of set-2, add it to result
                 (CONS first-elem rest-inter)
                 ;; Else, just return the intersection of the rest
                 rest-inter)))))

;; Set Intersection Tests
(FORMAT T "~%~%~60,,,'-A~%" "-")
(FORMAT T "~15A~A~%" "" "SET INTERSECTION TESTS")
(FORMAT T "~60,,,'-A~%~%" "-")

(FORMAT T "~2A~A~%" "" "INTERSECTION OPERATIONS:")
(FORMAT T "~4ATest 1: (set-intersection '(1 2 3) '(2 4)) => ~a~%" "" (set-intersection '(1 2 3) '(2 4)))
(FORMAT T "~4ATest 2: (set-intersection '(2 1 3) '(2 1 4)) => ~a~%" "" (set-intersection '(2 1 3) '(2 1 4)))
(FORMAT T "~4ATest 3: (set-intersection '(2 1 3) '(1 2 4)) => ~a~%" "" (set-intersection '(2 1 3) '(1 2 4)))
(FORMAT T "~4ATest 4: (set-intersection '(1 2) '(4)) => ~a~%" "" (set-intersection '(1 2) '(4)))
(FORMAT T "~4ATest 5: (set-intersection '(1 2) '(4 2)) => ~a~%" "" (set-intersection '(1 2) '(4 2)))

(FORMAT T "~%~60,,,'-A~%~%" "-")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the difference of set-1 and set-2.

;; The result should contain no duplicates.

;; Assume set-1 contains no duplicates and set-2 contains no duplicates.

;;

;; Examples:

;; (set-diff '(1 2) '(2 4)) => '(1)

  ;; GOAL
  ;; We want to return a list that contains the difference betweens sets one and two, that is that we want to
  ;; look at the first element of a list and compare it with every other element of the second list. If there
  ;; is no match we want to include the item in the output

  ;; BASE CASE
  ;; the base base is if set-one is empty

(defun set-diff (set-1 set-2)

  ;; HERE IS THE BASE CASE, that is that set-1 is empty. We want to return an empty list
  
  (IF (NULL set-1)
      
      NIL

      ;; Get the first element of set-1
      
      (LET ((first-elem (CAR set-1)))

        ;; We use set-member, a function we built earlier to detect if an element matches
        ;; IF the element matches we don't include it and therefore go to the next step
        ;; which is recursively calling the set-difference function
        
        (IF (set-member set-2 first-elem)
            (set-diff (CDR set-1) set-2)  ;; Skip this element if we DON"T find it
            (CONS first-elem (set-diff (CDR set-1) set-2))))))  ;; Otherwise we would like to keep it, it would be so epic

;; Set Difference Tests
(FORMAT T "~%~%~60,,,'-A~%" "-")
(FORMAT T "~15A~A~%" "" "SET DIFFERENCE TESTS")
(FORMAT T "~60,,,'-A~%~%" "-")

(FORMAT T "~2A~A~%" "" "DIFFERENCE OPERATIONS:")
(FORMAT T "~4ATest 1: (set-diff '(1 2 3) '(2 4)) => ~a~%" "" (set-diff '(1 2 3) '(2 4)))
(FORMAT T "~4ATest 2: (set-diff '(a b c) '(b d)) => ~a~%" "" (set-diff '(a b c) '(b d)))
(FORMAT T "~4ATest 3: (set-diff '(1 2 3) '(1 2 3)) => ~a~%" "" (set-diff '(1 2 3) '(1 2 3)))
(FORMAT T "~4ATest 4: (set-diff '(x y z) '()) => ~a~%" "" (set-diff '(x y z) '()))
(FORMAT T "~4ATest 5: (set-diff '() '(1 2 3)) => ~a~%" "" (set-diff '() '(1 2 3)))
(FORMAT T "~4ATest 6: (set-diff '(1 2 2 3) '(2)) => ~a~%" "" (set-diff '(1 2 2 3) '(2)))

(FORMAT T "~%~60,,,'-A~%~%" "-")

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

;; Boolean Expression Evaluator Tests
(FORMAT T "~%~%~60,,,'-A~%" "-")
(FORMAT T "~15A~A~%" "" "BOOLEAN EXPRESSION EVALUATOR TESTS")
(FORMAT T "~60,,,'-A~%~%" "-")

;; Base cases
(FORMAT T "~2A~A~%" "" "BASE CASES:")
(FORMAT T "~4ATest 1: (boolean-eval T) => ~a~%" "" (boolean-eval T))
(FORMAT T "~4ATest 2: (boolean-eval NIL) => ~a~%~%" "" (boolean-eval NIL))

;; NOT
(FORMAT T "~2A~A~%" "" "NOT OPERATOR:")
(FORMAT T "~4ATest 3: (boolean-eval '(NOT T)) => ~a~%" "" (boolean-eval '(NOT T)))
(FORMAT T "~4ATest 4: (boolean-eval '(NOT NIL)) => ~a~%~%" "" (boolean-eval '(NOT NIL)))

;; AND
(FORMAT T "~2A~A~%" "" "AND OPERATOR:")
(FORMAT T "~4ATest 5: (boolean-eval '(AND T T)) => ~a~%" "" (boolean-eval '(AND T T)))
(FORMAT T "~4ATest 6: (boolean-eval '(AND T NIL)) => ~a~%~%" "" (boolean-eval '(AND T NIL)))

;; OR
(FORMAT T "~2A~A~%" "" "OR OPERATOR:")
(FORMAT T "~4ATest 7: (boolean-eval '(OR T NIL)) => ~a~%" "" (boolean-eval '(OR T NIL)))
(FORMAT T "~4ATest 8: (boolean-eval '(OR NIL NIL)) => ~a~%~%" "" (boolean-eval '(OR NIL NIL)))

;; IMPLIES
(FORMAT T "~2A~A~%" "" "IMPLIES OPERATOR:")
(FORMAT T "~4ATest 9: (boolean-eval '(IMPLIES T NIL)) => ~a~%" "" (boolean-eval '(IMPLIES T NIL)))
(FORMAT T "~4ATest 10: (boolean-eval '(IMPLIES NIL T)) => ~a~%~%" "" (boolean-eval '(IMPLIES NIL T)))

;; IFF
(FORMAT T "~2A~A~%" "" "IFF OPERATOR:")
(FORMAT T "~4ATest 11: (boolean-eval '(IFF T T)) => ~a~%" "" (boolean-eval '(IFF T T)))
(FORMAT T "~4ATest 12: (boolean-eval '(IFF T NIL)) => ~a~%~%" "" (boolean-eval '(IFF T NIL)))

;; Nested
(FORMAT T "~2A~A~%" "" "NESTED EXPRESSIONS:")
(FORMAT T "~4ATest 13: (boolean-eval '(AND (OR T NIL) (AND T T))) => ~a~%" "" (boolean-eval '(AND (OR T NIL) (AND T T))))
(FORMAT T "~4ATest 14: (boolean-eval '(IFF (OR T NIL) (AND T T))) => ~a~%" "" (boolean-eval '(IFF (OR T NIL) (AND T T))))

(FORMAT T "~%~60,,,'-A~%~%" "-")



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
;; we have two pointers, one twice as fast as the other
;; we collect elements in one list while cutting off the "fast" list from the beginning

;; merge the two sorted lists
;; compare first elements of both lists
;; add the smallest element 

(defun merge-sort (list predicate)

  ;; BASE CASE
  ;; the if is part of the base case that checks either if the list is empty or that it only has one element.
  ;;
  
  (IF (OR (NULL list) (NULL (CDR list)))
      list

      ;; First we have to split the list using a helper function
      ;; we use LET* since up and down depends on parts otherwise the function blows up and it is very sad
      
      (LET* ((PART (split-list list))
             (up (FIRST part))
             (down (SECOND part)))
        
        ;; This is where the rubber meets the road. merge-up is a helper function that sorts atoms and lists
        ;; back "up"
        ;; we also pass in the predicate so that the lists are sorted in the correct order.

        
        (merge-up (merge-sort up predicate)
                  (merge-sort down predicate)
                  predicate))))




;; Here is the first helper function that takes two ordered lists and combines them into one
;; ordered list

;; RIGHT NOW THIS ONLY TAKES TWO SORTED SUB-LISTS AND SORTS THEM UP TO THE NEXT LEVEL
;; EX. (merge-up '(1 3 5) '(2 4 6) #'<)
;; EX. (merge-up '(9 6 3) '(8 5 2) #'>)

;; notes for tomorrow, split functionality

(defun merge-up (left right predicate)
  (COND

    ;; IF the left is empty, return the right list
    ;; IF the right is empty, return the left list
    ;; This is the base case
    
    ((NULL left) right)
    ((NULL right) left)

    ;; Here we use funcall to pass in the comparison
    ;; we recursively call merge-up to place the atoms correctly in the big list
    
    ((FUNCALL predicate (CAR left) (CAR right))
     (CONS (CAR left) (merge-up (CDR left) right predicate)))
    (T
     (CONS (CAR right) (merge-up left (CDR right) predicate)))))



;; Now we need something to split a given list into two lists from some middle point in the list
;; an important consideration is how we might split lists with an odd parity for the length
;; An example is that 5 would split 2 and 3 or 7 would be 4 and 3.

;; Here we will have what are essentially two pointers, we have two lists where one pointer
;; goes through the list twice as fast.

(defun split-list (list)
  ;; this function will return two lists
  ;; left  will  collect the first half of the list but in reverse order
  ;; right moves slowly through the list one step at a time
  ;; fast  moves fast through the list two steps at a time, twice the speed of right
  
  (LABELS ((split-helper (left right fast)

             ;; recursion stops if either fast is NIL (the end of the list, OR
             ;; (CDR fast) is NILL, meaning only one element is left
             ;; This is the base case
             
             (IF (OR (NULL fast) (NULL (CDR fast)))

                 ;; IF the base case is true we want to return the reverse of the left and also return
                 ;; the right list
                 
                 (LIST (REVERSE left) right)

                 ;; OTHERWISE
                 ;; move right forward by one
                 ;; move fast forward by two
                 ;; CONS takes an element from the right list and adds it to the left, we have to remember
                 ;; that left is going to be constructed in reverse order
                 ;; NEXT CDR right is going to go 1 step through the list
                 ;; CDR (CDR fast) is going to go through two steps through the list. This is how
                 ;; we will detect when we are at the end of the list
                 
                 (split-helper (CONS (CAR right) left)
                               (CDR right)
                               (CDR (CDR fast))))))
    
    (split-helper '()  list list)))




;; Merge Sort Tests
(FORMAT T "~%~%~60,,,'-A~%" "-")
(FORMAT T "~15A~A~%" "" "MERGE SORT TESTS")
(FORMAT T "~60,,,'-A~%~%" "-")

(FORMAT T "~2A~A~%" "" "ASCENDING ORDER SORTS:")
(FORMAT T "~4ATest 1: (merge-sort '(2 1 5 0) #'<) => ~a~%" "" (merge-sort '(2 1 5 0) #'<))
(FORMAT T "~4ATest 3: (merge-sort '() #'<) => ~a~%" "" (merge-sort '() #'<))
(FORMAT T "~4ATest 4: (merge-sort '(7) #'<) => ~a~%" "" (merge-sort '(7) #'<))
(FORMAT T "~4ATest 5: (merge-sort '(4 4 4 4) #'<) => ~a~%" "" (merge-sort '(4 4 4 4) #'<))
(FORMAT T "~4ATest 6: (merge-sort '(3 1 4 1 5 9 2) #'<) => ~a~%~%" "" (merge-sort '(3 1 4 1 5 9 2) #'<))

(FORMAT T "~2A~A~%" "" "DESCENDING ORDER SORTS:")
(FORMAT T "~4ATest 2: (merge-sort '(2 1 5 0) #'>) => ~a~%" "" (merge-sort '(2 1 5 0) #'>))
(FORMAT T "~4ATest 7: (merge-sort '(3 1 4 1 5 9 2) #'>) => ~a~%" "" (merge-sort '(3 1 4 1 5 9 2) #'>))

(FORMAT T "~%~60,,,'-A~%~%" "-")




;; SOME TEST FUNCTION
(defun myFun1 (num1 num2)
  (+ num1 num2)
  )

(defun myFun2 (set1 set2)
  (cons set1 set2)
  )

