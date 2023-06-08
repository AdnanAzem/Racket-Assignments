#lang pl 02


;;################### Q1 ###################
#|
In this language we have different AST for each parse of SE (ambguity)

######## Q1.a ########
The Grammar of the language

<SE> ::= <Str>          (1)
        |<D>            (2)
        |<Char>         (3)
        |{string <Char>}           (4)
        |{string-append <Str*> <str>}             (5)
        |{string-insert <Str> <Char> <D>}         (6)
        |{number->string <NUM>}          (7)
        |{string-length <str>}          (8)



#Represent an infinite digits for representing the number in a string
<D> ::= <Digit>        (9)
       |<Digit> <D>   (10)



# Represent the empty string
<Empty> ::= λ      (11)



# Represent a String form digits only
<Str> ::= <"<D>">        (12)
         |<Empty> <Str>       (13)
         |<Empty>       (14)
         |{string-insert <Str> <Char> <D>}         (15)
         |{number->string <NUM>}          (16)
         |{string <Char>}                 (17)


# Represnt a Number
<NUM> ::= <D>          (18)
         |{string-length <str>}        (19)



# Represent a char of one digit
<Char> ::= <CHR>         (20)
          |<CHR> <Char>         (21)



# Represnt characters from #\0 #\1 #\2 etc..
<CHR> ::= #\0 | #\1 | #\2 | #\3 | #\4 | #\5 | #\6 | #\7 | #\8 | #\9     (22)



# Represent digits from 0 - 9
<Digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9          (23)



# Represnt a terminal to avoid ambiguity in append so instead of using <Str> <Str> in append I used <Str*> <Str> to just make sure that there is one deriviative
<Str*> ::= {string Char}          (24)



######## Q1.b ########

# valid Example

# We have used the number of each grammar to help you to check my examples.

example No.1
"12344" => (12) (Str"D(1)
                      D(2)
                       D(3)
                        D(4)
                         D(4)") => "12344"


example No.2
(parse ({string #\1 #\2 #\4}) (4) => (String (21)^2 (20))
# here we call the (4) grammar then we call Char grammar first one is number (21) and the second one is (20)


example No.3
(parse {string-append {string #\1 #\2 #\4} "12"}) (5) => {string-append (24) "12"} => {string-append (24) "12"} => {string-append (24) (12)} => {string-append (24) (10) (9)}



example No.4
(parse (number->string(string-length "0033344"))) => {number->string{(18)}} => {number->string{(12)}} => {number->string{(10)}}
{number->string{(10)}} # and we got the result



# invalid Example

example No.1
"a2b" => Error because there is no A and B characters in our alphabet


example No.2
12 13 4 67 => Error because we don't have this word form in our grammar


example No.3
(string 124) = {(4)} => Error because we can't get numbers without #\v because we will go to CHR grammar so we don't have this kind of words.


example No.4
(string-append(string-length "44") "12") => Error because we don't have (string "44") in our Str grammar


example No.5
(string-insert "1357" 4 66) => Error because we can't apply that there is a terminal is missing

|#




;;################### Q2 ###################

; a helper function that calculate the square of number
; In this function we multiply each element , the function will be applied on each element on the list
(: square : Number -> Number)
(define (square num)
(* num num))


; In this function we are using lambda expression and we are calculating the result with map using folfl every time map will multiply the number
(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares lst)
  (let ([result (map square lst)]) ; Map function will do the following => result = 1*1 => result =1 (using foldl)  result += 2*2  result += 3*3 etc..
    (foldl + 0 result)))


;;################### Q2-Tests ###################
(test(sum-of-squares '(1 2 3)) => 14)
(test(sum-of-squares '(3 3 3)) => 27)
(test(sum-of-squares '(1 2 3 4)) => (+ 14 (* 4 4)))
(test(sum-of-squares '(5)) => 25)
(test(sum-of-squares '(2 5)) => 29)
(test(sum-of-squares '(1 1 1 1 1 1 1)) => 7)
(test(sum-of-squares '(1 1 1 1 1 1 1 5)) => (+ 7 (* 5 5)))
(test(sum-of-squares '()) => 0)
(test(sum-of-squares null) => 0)





;;################### Q3 ###################

;######## Q3.a ########
;; In this function we recieves a function and return a function
(: createPolynomial : (Listof Number) -> (Number -> Number))
(define (createPolynomial coeffs)
  (: poly : (Listof Number) Number Integer Number -> Number)
  (define (poly argsL x power accum)
    (if (null? argsL)  ; return the accum of the list if the list is null/empty
        accum
        (poly (rest argsL) x (+ power 1) (+ accum (* (first argsL) (expt x power))))))
  (: polyX : Number -> Number)  ; poly recieve an x
  (define (polyX x)
    (poly coeffs x 0 0))  ; call poly with this parameters to start the tail recursion
  polyX)  ; return the function


;;################### Tests From Assignment ###################

(define p2345 (createPolynomial '(2 3 4 5))) 
(test (p2345 0) =>  
   (+ (* 2 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5 
(expt 0 3)))) 
(test (p2345 4) =>  
   (+ (* 2 (expt 4 0)) (* 3 (expt 4 1)) (* 4 (expt 4 2)) (* 5 (expt 4 3)))) 
(test (p2345 11) => (+ (* 2 (expt 11 0)) (* 3 (expt 11 1)) (* 4 (expt 11 2)) (* 5 (expt 11 3)))) 
 
 
(define p536 (createPolynomial '(5 3 6))) 
(test (p536 11) => (+ (* 5 (expt 11 0)) (* 3 (expt 11 1)) (* 6 
(expt 11 2)))) 
 
(define p_0 (createPolynomial '())) 
(test (p_0 4) => 0)


;######## Q3.b ########
#|
The Grammar:
 <PLANG> ::= {{poly <AEs>}{<AEs>}}  # The PLANG terminal
 
 <AEs> ::= <AE>
          |<AE> <AEs>  

 <AE> ::= <num>
         |<num> + <AE>
         |<num> - <AE>

|#

(define-type PLANG
  [Poly (Listof AE) (Listof AE)])

;; This code was provided for us in the assignment
(define-type AE
  [Num  Number] 
  [Add  AE AE] 
  [Sub  AE AE] 
  [Mul  AE AE] 
  [Div  AE AE]) 
 
  (: parse-sexpr : Sexpr -> AE) 
  ;; to convert s-expressions into AEs 
  (define (parse-sexpr sexpr) 
    (match sexpr 
      [(number: n)    (Num n)] 
      [(list '+ lhs rhs) (Add (parse-sexpr lhs)                     
       (parse-sexpr rhs))] 
      [(list '- lhs rhs) (Sub (parse-sexpr lhs)
       (parse-sexpr rhs))] 
      [(list '* lhs rhs) (Mul (parse-sexpr lhs)  
       (parse-sexpr rhs))] 
      [(list '/ lhs rhs) (Div (parse-sexpr lhs)  
       (parse-sexpr rhs))] 
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)])) 



  (: parse : String -> PLANG) 
  ;; parses a string containing a PLANG expression to a PLANG AST
    (define (parse str) 
    (let ([code (string->sexpr str)]) 
      (match code
        ;; Throw error if the first part is empty.
        [(list (cons 'poly '()) (list t ...)) (error 'parse "at least one coefficient is required in ~s" code)]
        ;; Throw error if the second part is empty.
        [(list (cons 'poly h) '()) (error 'parse "at least one point is required in ~s" code)]
        ;; Else we assume a correct syntax and use 2 maps to parse each of list.
        [(list (cons 'poly h) (list t ...)) (Poly (map parse-sexpr h) (map parse-sexpr t))]
        [else (error 'parse "bad syntax in ~s" code)])))


;;################### Tests From Assignment ###################

(test (parse "{{poly 1 2 3} {1 2 3}}")  
     => (Poly (list (Num 1) (Num 2) (Num 3))  
              (list (Num 1) (Num 2) (Num 3)))) 
(test (parse "{{poly } {1 2} }")  
     =error> "parse: at least one coefficient is required in ((poly) (1 2))") 
(test (parse "{{poly 1 2} {} }") =error> "parse: at least one point is required in ((poly 1 2) ())") 


;; evaluates AE expressions to numbers
(: eval : AE -> Number)
(define (eval expr)
  (cases expr 
    [(Num n)  n] 
    [(Add l r) (+ (eval l) (eval r))] 
    [(Sub l r) (- (eval l) (eval r))] 
    [(Mul l r) (* (eval l) (eval r))] 
    [(Div l r) (/ (eval l) (eval r))]))


(: eval-poly : PLANG ->  (Listof Number) )
#|
 1) Check if the type is poly
 2) Evaluate all AEs and create polynom
 3) Use the return function to calculate value at poits with 3 maps
|#
(define (eval-poly p-expr)
  (cases p-expr
  [(Poly coeffs points) (map (createPolynomial (map eval coeffs)) (map eval points))])) 
 
(: run : String -> (Listof Number)) 
  ;; evaluate a FLANG program contained in a string 
  (define (run str) 
    (eval-poly (parse str))) 


;;################### Tests From Assignment ###################

(test (run "{{poly 1 2 3} {1 2 3}}")  
=> '(6 17 34)) 
(test (run "{{poly 4 2 7} {1 4 9}}")  => '(13 124 589)) 
(test (run "{{poly 1 2 3} {1 2 3}}")   => '(6 17 34)) 
(test (run "{{poly 4/5 } {1/2 2/3 3}}")  
=> '(4/5 4/5 4/5)) 
(test (run "{{poly 2 3} {4}}")  => '(14)) 
(test (run "{{poly 1 1 0} {-1 3 3}}")  => '(0 4 4))  
(test (run "{{poly {/ 4 2} {- 4 1}} {{- 8 4}}}") 
=> '(14)) 
(test (run "{{poly {+ 0 1} 1 {* 0 9}} {{- 4 5} 3 
{/ 27 9}}}") 
=> '(0 4 4)) 


