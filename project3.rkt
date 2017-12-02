#lang racket
(require racket/trace)

;LIST FUNCTIONS

(displayln "
type (displayListFunctions) to see the list functions")

(displayln "
type (displaySetFunctions) to see the set functions")

(displayln "
type (displayMathFunctions) to see the math functions")

(displayln "
type (displayRequiredFunctions) to see required functions")

(define(displayListFunctions)
  (displayln "1. append two lists - (app list list)")
  (displayln "2. reverse a list - (reverse list)")
  (displayln "3. merge two lists - (merge list list)")
  (displayln "4. add to end - (addEnd  list)")) 

(define(displaySetFunctions)
  (displayln "5. set membership - (member value list)")
  (displayln "6. insert an element to a set - (insert value list)")
  (displayln "7. intersection: show common elements in two lists - (intersection list list)")
  (displayln "8. cardinality: show the number of elements in a set - (cardinality list)")
  (displayln "9. union: combine two sets - (union list list)")
  (displayln "10. difference: show what elements in the first set are not in the second - (difference list list)")
  (displayln "11. subset: shows if the first set is a subset of the second - (subset? list list)") 
  (displayln "12. superset: shows if the first set is a superset of the second - (superset? list list)")) 
  

(define(displayMathFunctions)
  (displayln "13. factorial: outputs the factorial of some number - (factorial #)")
  (displayln "14. fibonacci: print nth fibonacci number - (nth fibo #)")
  (displayln "15. gcd: outputs the greatest common denominator of two numbers - (gcd # #)")
  (displayln "16. abs: outputs the absolute value of a number - (abs #)")
  (displayln "17. prime: says whether or not a number is prime - (prime? #)")
  (displayln "18. lcm : outputs the least common multiple of two numbers - (lcm # #)")
  (displayln "19. right triangle: says whether or not three numbers add up to form the sides of a right triangle - (rTriangle? # # #)"))

(define(displayRequiredFunctions)
  (displayln "20. perfect: says whether or not a numbers factors add up to the number - (perfect? #)")
  (displayln "21. abundant: says if the sum of a numbers factors are greater than the number (abundant? #)")
  (displayln "22. deficient: says if the sum of a numbers factors are less than the number (deficient? #)"))

(define (app list1 list2) ;appends list 2 to end of list 1
  (if (null? list1)       ; if list 1 is null, return list 2
      list2
      (cons (car list1)(app(cdr list1)list2))));make a list that is the first elements of list1 appended to

;reverse function
(define(rev lst)
  (if(null? lst)
     null
     (app (rev (cdr lst)) (list (car lst))))) ;append first elem to the rest of the list after reversed


;merge function
(define(merge list1 list2)
  (if(null? list1)
     list2 ;first list empty
     (if(null? list2)
        list1 ;second list empty
        (if(< (car list1) (car list2)) ;if first val of list1 <first val of list2
           (cons (car list1) (merge (cdr list1) list2)) ;make a list starting w first val of list1 and merge rest of lists
           (cons (car list2) (merge list1 (cdr list2)))))))

;add to end function
(define(addEnd var lst)
  (if (null? lst)
      var
      (rev (cons var (rev lst)))))


;SET FUNCTIONS
;member function
(define (member val list1)
  (if(null? list1)
     #f ;cannot be member of empty list
     (if(equal? (car list1) val)
        #t ;if values are equal true
        (member val (cdr list1))))) ;test rest of list

;insert function
(define(insert val list1)
  (if(member val list1)
     list1
     (cons val list1)))

;intersection function
(define (intersection list1 list2)
  (if(null? list1)
     list1
     (if(member (car list1) list2) ;if there is a common value
       (cons (car list1) (intersection(cdr list1) list2));make a list including that value, w intersection of rest of lists
       (intersection(cdr list1) list2)))) ;if not a common val, test rest of lists

;difference function
(define (difference list1 list2)
  (if(null? list1)
     list1
     (if(member (car list1) list2) ;if there is a common value
        (difference (cdr list1) list2) ;ignore it and keep finding the difference
        (cons(car list1) (difference (cdr list1) list2))))) ;if it is not common, add it to the difference list

;subset function
(define(subset? list1 list2)
  (if(null? list1)
     #t
     (if(member (car list1) list2)
        (subset? (cdr list1) list2)
        #f)))

;superset function
(define(superset? list1 list2)
  (if(subset? list2 list1)
     #t
     #f))

;cardinality function
(define (cardinality list)
  (if(null? list)
     0
     (+ (cardinality (cdr list)) 1)));increase every time there is another element in the list

;union function
(define (union lst1 lst2)
   (if (null? lst1)
        lst2
        (if(member (car lst1) lst2)
           (union (cdr lst1) lst2)
           (cons(car lst1) (union (cdr lst1) lst2)))))
           
;MATH FUNCTIONS

;factorial function
(define(factorial num)
  (if (< num 2) ;base case
      1
      (* num (factorial(- num 1))))) ; multiply num by each preceding num

;fibonacci function
(define(nth-fibo num)
  (if(= num 1) ;if first fib 
     1
     (if(= num 2) ;if second fib
        1
        (+ (nth-fibo(- num 1)) (nth-fibo(- num 2)))))) ; else add prev two fib

;gcd function
(define (gcd num1 num2)
  (if (= num1 num2) ;if the numbers are equal, return num1
      num1
      (if(< num1 num2) ;if num1 less than num2
         (gcd num1 (- num2 num1)) ; recurse on num1 (num2-num1)
         (gcd (- num1 num2) num2)))) ; else recurse on (num1-num2) num2

;absolute value function
(define(abs val)
  (if(< val 0) ;if negative number
     (* val -1) ;return positive number
     val))

;prime number function
(define(prime? val)
  (isPrime 2 val))

(define(isPrime check num)
  (if(= check num)
     #t
     (if(= num 1)
        #t
        (if(= (modulo num check) 0); if this check is a factor of num tested, false
           #f
           (isPrime (+ check 1) num))))) ;keep testing until we reach num

;least common multiple function

(define (lcm val1 val2)
  (/(* val1 val2)(gcd val1 val2)))

;sides of a right triangle function

(define (rTriangle? val1 val2 val3)
  (if (= (+ (* val1 val1) (* val2 val2)) (* val3 val3))
      #t
      #f))


; REQUIRED FUNCTIONS

(define (isFactor s l) (= (modulo l s) 0)) ;a number is a factor of another if its remainder is 0

(define(findSum check val sum)
    (if(= check 1) ;if we are at the end of the check
       (+ sum 1) ;add 1 to sum
       (if(isFactor check val)
          (findSum (- check 1) val (+ sum check)) ;if it is a factor, add its sum and keep checking
          (findSum (- check 1) val sum)))) ;else keep checking
                   
(define(perfect? val)
  (if(= (findSum (- val 1) val 0) val)
     #t
     #f))

(define(abundant? val)
  (if(> (findSum (- val 1) val 0) val)
     #t
     #f))

(define(deficient? val)
  (if(< (findSum (- val 1) val 0) val)
     #t
     #f))

(define (displayRequired)
  (displayln "1. Check if a number is perfect - (perfect? #)")
  (displayln "2. Check if a number is abundant - (abundant? #)")
  (displayln "3. Check if a number is deficient - (deficient? #)"))


;(displayln "1. Check if a number is perfect - (perfect? #)
; |   Check if a number is abundant - (abundant? #)
 ; |   Check if a number is deficient - (deficient? #)")