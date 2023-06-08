#lang pl


;;############## Q1-a ##############

#|
in this function we get a list of lists
and then we merge the lists to one list
|#
(: open-list : (Listof (Listof Number)) -> (Listof Number))
(define (open-list lst)
  (cond [(null? lst) null]
        [else (append (first lst) (open-list (rest lst)))]
        ))

;;############## Q1-a Tests ##############
(test (open-list '((1 2 3) (2 3 3 4) (9 2 -1) (233 11
90))) => '(1 2 3 2 3 3 4 9 2 -1 233 11 90))
(test(open-list null) => null)
(test (open-list '((1 2) (88 23 44))) => '(1 2 88 23 44))
(test (open-list '((1 2))) => '(1 2))
(test (open-list '((1) (4) (22) (83))) => '(1 4 22 83))
(test(open-list '()) => null)
(test(open-list '((2 65 77) () (5 1))) => '(2 65 77 5 1))



;;############## Q1-b ##############

#|
in this function we get a list of lists
and then we return the minimum number and maximum number
in all the lists (Real Numbers)
|#
(: min&max : (Listof (Listof Number)) -> (Listof Number))
(define (min&max lst)
  (cond [(null? lst) '(-inf.0 +inf.0)]
        [else (sort(cons (minimum (open-list lst)) (cons (maximum (open-list lst)) '())) <)]  
  ))

#|
a helper function to get the minimum number in the list
by using the function min
|#
(: minimum : (Listof Number) -> Number)
(define (minimum lst)
  (cond [(null? lst) +inf.0]
        [else (min (first lst) (minimum(rest lst)))]
        ))

#|
a helper function to get the maximum number in the list
by using the function max
|#
(: maximum : (Listof Number) -> Number)
(define (maximum lst)
  (cond [(null? lst) -inf.0]
        [else (max (first lst) (maximum(rest lst)))]))


;;############## Q1-b Tests ##############
(test (min&max '((1 2 3) (2 3 3 4) (9 2 -1) (233 11
90))) => '(-1.0 233.0))
(test (min&max null) => '(-inf.0 +inf.0))
(test (min&max '((1 1) (2 5))) => '(1.0 5.0))
(test (min&max '((1 1))) => '(1.0 1.0))
(test (min&max '((15))) => '(15.0 15.0))
(test (min&max '((15) (7))) => '(7.0 15.0))
(test (min&max '()) => '(-inf.0 +inf.0))
(test (min&max '((19 1) ())) => '(1.0 19.0))
(test (min&max '( () )) => '(-inf.0 +inf.0))
(test (min&max '((1 41.7) (0.3 15))) => '(0.3 41.7))
(test (min&max '((1/2 1) (2 5))) => '(0.5 5.0))

;;############## Q1-c ##############

#|
a helper function to get the minimum number in the list
by using the function  apply min
|#
(: apply_minimum : (Listof Number) -> Number)
(define (apply_minimum lst)
  (cond [(null? lst) +inf.0]
        [else (apply min lst)]
        ))

#|
a helper function to get the maximum number in the list
by using the function apply max
|#
(: apply_maximum : (Listof Number) -> Number)
(define (apply_maximum lst)
  (cond [(null? lst) -inf.0]
        [else (apply max lst)]
        ))
#|
in this function we get a list of lists
and then we return the minimum number and maximum number
in all the lists and we use apply function to find
the min number and max number(Integer Numbers)
|#

(: min&max_apply : (Listof (Listof Number)) -> (Listof Number))
(define (min&max_apply lst)
  (cond [(null? lst) '(-inf.0 +inf.0)]
        [else (sort(cons(apply_minimum (open-list lst)) (cons(apply_maximum(open-list lst)) '())) <) ]
  ))

;;############## Q1-c Tests ##############
(test (min&max_apply '((1 2 3) (2 3 3 4) (9 2 -1) (233
11 90))) => '(-1 233))
(test (min&max_apply null) => '(-inf.0 +inf.0))
(test (min&max_apply '((1 1) (2 5))) => '(1 5))
(test (min&max_apply '((1 1.1))) => '(1.0 1.1))
(test (min&max_apply '((15))) => '(15 15))
(test (min&max_apply '((15) (7))) => '(7 15))
(test (min&max_apply '((17 13) ())) => '(13 17))
(test (min&max_apply '( () )) => '(-inf.0 +inf.0))
(test (min&max_apply '((1/2 1) (2 55.7))) => '(0.5 55.7))


;;############## Q2.1 + Q2.2 ##############
(define-type Table [EmptyTbl] [Add Symbol String Table])


;;############## Q2.3 ##############
#|
in this function we get symbol and table as inputs
and we ruturn false if the symbol does not exists or the Table null
else we return the first value
|#
(: search-table : Symbol Table -> (U String Boolean))
(define (search-table symb tbl)
  (cases tbl
    [(EmptyTbl) #f]
    [(Add key value tb)
     (cond [(equal? symb key) value]
           [else (search-table symb tb)])]))

;;############## Q2.4 ##############
#|
in this function we get Table and symbol as inputs
and we return (EmptyTbl) if the Table is null
else we return a new table contains the items of the original table
except of the item to be deleted without the first value.
|#
(: remove-item : Table Symbol -> (U Table))
(define (remove-item tbl symb)
  (cases tbl
    [(EmptyTbl) (EmptyTbl)]
    [(Add key value tb)
     (cond [(equal? key symb) tb]
           [else (Add key value (remove-item tb symb))])])
  )


;;############## Q2.2 Tests ##############
(test (Add 'b "B" (Add 'a "A" (EmptyTbl))) =>
      (Add 'b "B" (Add 'a "A" (EmptyTbl))))

(test (Add 'a "ADNAN" (Add 'y "AZEM" (EmptyTbl)))
      =>(Add 'a "ADNAN" (Add 'y "AZEM"(EmptyTbl))))

(test (EmptyTbl) => (EmptyTbl))
(test (Add 'h "H" (Add 'i "i" (EmptyTbl))) => (Add 'h "H" (Add 'i "i" (EmptyTbl))))
(test (Add 'y "Y" (Add 'o "O" (Add 'u "U" (EmptyTbl)))) => (Add 'y "Y" (Add 'o "O" (Add 'u "U" (EmptyTbl)))))


;;############## Q2.3 Tests ##############
(test (search-table 'a (Add 'a "ADNAN" (Add 'y "AZEM" (Add 'e ";)" (EmptyTbl))))) => "ADNAN")
(test (search-table 'a (Add 'a "A" (Add 'b "B" (Add 'a "AAA" (EmptyTbl))))) => "A")
(test (search-table 'd (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl))))) => #f)
(test (search-table 'd (Add 'n "Never" (Add 'b "back" (Add 'd "down" (EmptyTbl))))) => "down")
(test (search-table 'w (Add 'n "Never" (Add 'b "back" (Add 'd "down" (EmptyTbl))))) => #f)
(test (search-table 'w (EmptyTbl)) => #f)



;;############## Q2.4 Tests ##############
(test (remove-item (EmptyTbl) 's) => (EmptyTbl))
(test (remove-item (Add 'a "ADNAN" (Add 's "SHADI" (Add 'a "AZEM" (EmptyTbl)))) 's) => (Add 'a "ADNAN" (Add 'a "AZEM" (EmptyTbl))))
(test (remove-item (Add 'a "ADNAN" (Add 'y "AZEM" (EmptyTbl))) 'a ) =>  (Add 'y "AZEM" (EmptyTbl)))
(test (remove-item (Add 'y "yes" (Add 'w "we" (Add 'c "can" (EmptyTbl)))) 'c) => (Add 'y "yes" (Add 'w "we" (EmptyTbl))))
(test (remove-item (Add 'a "ADNAN" (Add 's "SHADI" (Add 'a "AZEM" (EmptyTbl)))) 'a) => (Add 's "SHADI" (Add 'a "AZEM" (EmptyTbl))))
(test (remove-item (Add 'a "ADNAN" (Add 's "SHADI" (Add 'a "AZEM" (EmptyTbl)))) 'r) => (Add 'a "ADNAN" (Add 's "SHADI" (Add 'a "AZEM" (EmptyTbl)))))


;;############## Q2 Tests That in the Assignment ##############
(test (EmptyTbl) => (EmptyTbl))
(test (Add 'b "B" (Add 'a "A" (EmptyTbl))) =>
(Add 'b "B" (Add 'a "A" (EmptyTbl))))
(test (Add 'a "aa" (Add 'b "B" (Add 'a "A" (EmptyTbl)))) =>
(Add 'a "aa" (Add 'b "B" (Add 'a "A" (EmptyTbl)))))

(test (search-table 'c (Add 'a "AAA" (Add 'b "B" (Add 'a "A"
(EmptyTbl)))))
=> #f)
(test (search-table 'a (Add 'a "AAA" (Add 'b "B" (Add 'a "A"
(EmptyTbl)))))
=> "AAA")

(test (remove-item (Add 'a "AAA" (Add 'b "B" (Add 'a "A"
(EmptyTbl)))) 'a)
=> (Add 'b "B" (Add 'a "A" (EmptyTbl))))


(test (remove-item (Add 'a "AAA" (Add 'b "B" (Add 'a "A"
(EmptyTbl)))) 'b)
=> (Add 'a "AAA" (Add 'a "A" (EmptyTbl))))


