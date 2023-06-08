
#lang pl

;; ############### Expanding the FLANG BNF language ###############

;; FLANG BNF languge
#| The grammar:
 <FLANG> ::= <num> ;; Rule 1
            | { + <FLANG> <FLANG> } ;; Rule 2
            | { - <FLANG> <FLANG> } ;; Rule 3
            | { * <FLANG> <FLANG> } ;; Rule 4
            | { / <FLANG> <FLANG> } ;; Rule 5
            | { with { <id> <FLANG> } <FLANG> } ;; Rule 6
            | <id> ;; Rule 7
            | { fun { <id> } <FLANG> } ;; Rule 8
            | { call <FLANG> <FLANG> } ;; Rule 9
            | True ;; add rule for True ;; Rule 10
            | False ;; Rule 11
            | { = <FLANG> <FLANG>} ;; add rule for = ;; Rule 12
            | { > <FLANG> <FLANG>} ;; Rule 13
            | { < <FLANG> <FLANG>} ;; Rule 14
            | { not <FLANG>} ;; Rule 15
            | {if <FLANG> {then-do <FLANG>} {else-do <FLANG>}} ;; add rule 16 for (the above) if
expressions
|#


;; ############### Extending the Parser ###############

#|
In this part we completed the missing parts of the FLANG type definition.
|#
  (define-type FLANG
    [Num  Number]
    [Add  FLANG FLANG]
    [Sub  FLANG FLANG]
    [Mul  FLANG FLANG]
    [Div  FLANG FLANG]
    [Id   Symbol]
    [With Symbol FLANG FLANG]
    [Fun  Symbol FLANG]
    [Call FLANG FLANG]
    [Bool Boolean]
    [Bigger FLANG FLANG]
    [Smaller FLANG FLANG]
    [Equal FLANG FLANG]
    [Not FLANG]
    [If FLANG FLANG FLANG])


#|
In this function we get as input Sexpr and we return as output FLANG.
|#
(: parse-sexpr : Sexpr -> FLANG)
;; to convert s-expressions into FLANGs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)(Num n)]
    ;; when we get True symbol we returns #t.
    ['True (Bool true)]
    ;;when we get False symbol we return #f.
    ['False (Bool false)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'fun more)
     (match sexpr
       [(list 'fun (list (symbol: name)) body)
        (Fun name (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'call fun arg) (Call (parse-sexpr fun) (parse-sexpr arg))]
    ;; We wrote the expressions of binary operations like expressions of arithmetic operations
    [(list '= lhs rhs) (Equal(parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '> lhs rhs)(Bigger(parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '< lhs rhs)(Smaller(parse-sexpr lhs) (parse-sexpr rhs))]
    ;; when we get not symbol we do the same like when get number
    [(list 'not exp) (Not(parse-sexpr exp))]
    ;; when we get an expression 'if that consist three arguments we convert this expressions into FLANGs.
    [(cons 'if more)
     (match sexpr
       [(list 'if expr (list 'then-do body1) (list 'else-do body2))
        (If (parse-sexpr expr) (parse-sexpr body1) (parse-sexpr body2))]
       [else (error 'parse-sexpr "bad `if' syntax in ~s" sexpr)])]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))



#|
In this function we get input variable type string and return as output FLANG.
|#
  (: parse : String -> FLANG)
  ;; parses a string containing a FLANG expression to a FLANG AST
  (define (parse str)
    (parse-sexpr (string->sexpr str)))


;; ############### Extending subst and eval ###############
#|
Evaluation rules:
    subst:
      N[v/x]                = N
      {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
      {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
      {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
      {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]} ; if y =/= x
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
      {call E1 E2}[v/x]     = {call E1[v/x] E2[v/x]}
      {fun {y} E}[v/x]      = {fun {y} E[v/x]}           ; if y =/= x
      {fun {x} E}[v/x]      = {fun {x} E}
      B[v/x] = B ;; B is Boolean
      {= E1 E2}[v/x] = {= E1[v/x] E2[v/x]}
      {> E1 E2}[v/x] = {> E1[v/x] E2[v/x]}
      {< E1 E2}[v/x] = {< E1[v/x] E2[v/x]}
      { not E}[v/x] = {not E[v/x]}
      {if Econd {then-do Edo} {else-do Eelse}}[v/x] = {if Econd[v/x] {then-do Edo[v/x]} {else-do Eelse[v/x]}}


    eval:
      eval(N)            = N
      eval({+ E1 E2})    = eval(E1) + eval(E2)  \ if both E1 and E2
      eval({- E1 E2})    = eval(E1) - eval(E2)   \ evaluate to numbers
      eval({* E1 E2})    = eval(E1) * eval(E2)   / otherwise error!
      eval({/ E1 E2})    = eval(E1) / eval(E2)  /
      eval(id)           = error!
      eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
      eval(FUN)          = FUN ; assuming FUN is a function expression
      eval({call E1 E2}) = eval(Ef[eval(E2)/x]) if eval(E1)={fun {x}Ef}
                         = error!               otherwise
      eval(B) = B ;; B is an expression for a Boolean value
      eval({= E1 E2}) = eval(E1) = eval(E2) \ if both E1 and E2
      eval({> E1 E2}) = eval(E1) > eval(E2) \ evaluate to numbers
      eval({< E1 E2}) = eval(E1) < eval(E2) / otherwise error!
      eval({not E}) = not(eval(E)) /E may be anything
      eval({if Econd {then-do Edo} {else-do Eelse}})
                                                      = eval(Edo) if eval(Econd) =/= false,
                                                                     eval(Eelse),     otherwise.
|#


#|
In this function receives FLANG, Symbol and FLANG and returns FLANG.
|#

(: subst : FLANG Symbol FLANG -> FLANG)
  (define (subst expr from to)
    (cases expr
      [(Num n) expr]
      [(Add l r) (Add (subst l from to) (subst r from to))]
      [(Sub l r) (Sub (subst l from to) (subst r from to))]
      [(Mul l r) (Mul (subst l from to) (subst r from to))]
      [(Div l r) (Div (subst l from to) (subst r from to))]
      [(Id name) (if (eq? name from) to expr)]
      [(With bound-id named-expr bound-body)
       (With bound-id
             (subst named-expr from to)
             (if (eq? bound-id from)
               bound-body
               (subst bound-body from to)))]
      [(Call l r) (Call (subst l from to) (subst r from to))]
      [(Fun bound-id bound-body)
       (if (eq? bound-id from)
         expr
         (Fun bound-id (subst bound-body from to)))]
      ;; We wrote Bool like we wrote Num
      [(Bool b) expr]
      ;; We wrote the expressions of binary operations like expressions of arithmetic operations.
      ;; and in the same way we wrote "Not" and "IF"
      [(Equal l r) (Equal (subst l from to) (subst r from to))]
      [(Bigger l r) (Bigger (subst l from to) (subst r from to))]
      [(Smaller l r) (Smaller (subst l from to) (subst r from to))]
      [(Not n) (Not (subst n from to))]
      [(If exp1 exp2 exp3) (If (subst exp1 from to) (subst exp2 from to) (subst exp3 from to))]))




;; The following function is used in multiple places below,
;; hence, it is now a top-level definition
(: Num->number : FLANG -> Number)
;; gets a FLANG -- presumably a Num variant -- and returns the
;; unwrapped number
(define (Num->number e)
(cases e
[(Num n) n]
[else (error 'Num->number "expected a number, got: ~s" e)]))


(: arith-op : (Number Number -> Number) FLANG FLANG -> FLANG)
;; gets a Racket numeric binary operator, and uses it within a FLANG
;; `Num' wrapper
(define (arith-op op expr1 expr2)
(Num (op (Num->number expr1) (Num->number expr2))))


#|
In this function We do the same code from the "arith-op" function, and did Cast to Bool.
|#
(: logic-op : (Number Number -> Boolean) FLANG FLANG -> FLANG)
;; gets a Racket Boolean binary operator (on numbers), and applies it
;; to two `Num' wrapped FLANGs
(define (logic-op op expr1 expr2)
(Bool (op (Num->number expr1) (Num->number expr2))))


#|
In this function we receives FLANG as Input and returns Boolean as Output.
|#

(: flang->bool : FLANG -> Boolean)
;; gets a Flang E (of any kind) and returns a its appropiate
;; Boolean value -- which is true if and only if E does not
;; represent false
;; Remark: the `flang->bool` function will also be top-level
;; since it's used in more than one place.
(define (flang->bool e)
;; In RACKET any expression that is not false is considered true.
(cases e
;; If our FLANG is a Bool form then we will check if it is True or False and return accordingly.
[(Bool e) (if e #t #f)]
;; else, we will return True.
[else #t]))

#|
In this function we receives FLANG as Input and return FLANG as Output.
|#

 (: eval : FLANG -> FLANG)
  ;; evaluates FLANG expressions by reducing them to *expressions*
  (define (eval expr)
    (cases expr
      [(Num n) expr]
      [(Add l r) (arith-op + (eval l) (eval r))]
      [(Sub l r) (arith-op - (eval l) (eval r))]
      [(Mul l r) (arith-op * (eval l) (eval r))]
      [(Div l r) (arith-op / (eval l) (eval r))]
      [(With bound-id named-expr bound-body)
       (eval (subst bound-body
                    bound-id
                    (eval named-expr)))]
      [(Id name) (error 'eval "free identifier: ~s" name)]
      [(Fun bound-id bound-body) expr]
      [(Call fun-expr arg-expr)
       (let([fval (eval fun-expr)])
         (cases fval
           [(Fun bound-id bound-body)
            (eval (subst bound-body
                         bound-id
                         (eval arg-expr)))]
           [else (error 'eval "`call' expects a function, got: ~s" fval)]))]
     ;; we wrote Bool like we wrote Num
     [(Bool b)  expr]
     ;; We wrote the expressions of binary operations like expressions of arithmetic operations.
     ;; In this case we just use logic-op function instead of arith-op function
     [(Equal l r) (logic-op = (eval l) (eval r))]
     [(Bigger l r) (logic-op > (eval l) (eval r))]
     [(Smaller l r) (logic-op < (eval l) (eval r))]
     ;; we took the lef expression of the "If" did "eval" function on it.
     ;; then we send it to flang->bool function to get a boolean value from it.
     ;; then we made "If" condition and returned the appropriate "eval" according to the Boolean value of the left expression.
     [(If l m r)
      (let ([fval (eval l)])
        (if(flang->bool fval) (eval m) (eval r)))]
     ;; we send the exp to flang->bool function which returns us a boolean value
     ;; Then, we used the saved word in RACKET - "not" and we sent it to the Bool constructor.
     [(Not exp) (Bool(not(flang->bool (eval exp))))]))


;; ############### Extending the run procedure ###############

#|
In this function we receives String as input and returns
Number or Boolean or FLANG.
|#

(: run : String -> (U Number Boolean FLANG))
;; evaluate a FLANG program contained in a string
(define (run str)
(let ([result (eval (parse str))])
(cases result
[(Num n) n]  ;; if we receives Num we returns Number
[(Bool b) b]  ;; if we receives Bool we returns Boolean
[else result])))  ;; else we returns FLANG



;; ############### Tests From Assignment ###############
(test (run "True") => true)
(test (run "{not True}") => false)
(test (run "{> 3 44}") => false)
(test (run "{if {- 3 3} {then-do 4} {else-do 5}}") => 4)
(test (run "{with {x 8}
 {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 1/4)
(test (run "{with {x 0}
 {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 0)
(test (run "{if {> 2 1} {then-do True} {else-do {+ 2 2}}}") => true)
(test (run "{with {c True}
 {if c {then-do {> 2 1}} {else-do 2}}}")
 => true)
(test (run "{with {foo {fun {x}
 {if {< x 2} {then-do x} {else-do {/ x 2}}}}} foo}")
 => (Fun 'x (If (Smaller (Id 'x) (Num 2)) (Id 'x) (Div (Id 'x) (Num 2)))))
(test (run "{with {x 0}
 {if {> x 0} {/ 2 x} x}}")
 =error> "parse-sexpr: bad `if' syntax in (if (> x 0) (/ 2 x) x)")
 (test (run "true") =error> "eval: free identifier: true")
(test (run "{< false 5}") =error> "eval: free identifier: false")
(test (run "{< False 5}")
 =error> "Num->number: expected a number, got: #(struct:Bool #f)")



;; ############### Our Tests ###############
(test (run "{with {{x y} x}}") =error> "bad `with' syntax in")
(test (run "{= 199 37}") => false)
(test (run "{with {x 7} {if {> x 0} {then-do {* x x}} {else-do x}}}") => 49)
(test (run "{call {fun {x} {* x 3}} 4}") => 12)
(test (run "{call {fun {x} {- x 10}} 4}") => -6)
(test (run "{with {x 1} {if {= x 4} {then-do {* 2 x}} {else-do x}}}") => 1)
(test (run "{with {x 3} {if {< x 7} {then-do {* 10 x}} {else-do x}}}") => 30)
(test (run "{with {x 4} {if {not{< x 8}} {then-do {* 9 x}} {else-do x}}}") => 4)
(test (run "{with {x 2} {if {x} {then-do {* 9 x}} {else-do x}}}") =error> "parse-sexpr: bad syntax in (x)")
(test (run "{with {x 7} {with {y x} y}}") => 7)
(test (run "{with {x 10} {with {x x} x}}") => 10)
(test (run "{call {fun {7} {- x 4}} x}") =error> "parse-sexpr: bad `fun' syntax in (fun (7) (- x 4))")
(test (run "{call {fun {13} {- x 10}}{- x 22}}") =error> "parse-sexpr: bad `fun' syntax in (fun (13) (- x 10))") 
(test (run "{call {fun {5} {- x 1}} 2}") =error> "parse-sexpr: bad `fun' syntax in")
(test (run "{call {with {x 5} {with {x x} x}} 5}") =error> "eval: `call' expects a function, got: #(struct:Num 5)")
(test (run "{with {x 99} {if {= x 99} {then-do True} {else-do x}}}") => #t)
(test (run "{with {x 10} {if {not{< x 1}} {then-do {* 6 x}} {else-do x}}}") => 60)




