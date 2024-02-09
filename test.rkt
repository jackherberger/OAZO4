#lang typed/racket
(require typed/rackunit)

;Arith - language definition
;expressions
(define-type ExprC (U NumC BinopC IdC Ifleq0C AppC))
(struct NumC    ([n : Real]) #:transparent)                                   ;numbers
(struct BinopC  ([operation : Symbol] [l : ExprC] [r : ExprC]) #:transparent) ;+-*/
(struct AppC    ([fun : Symbol] [arg : (Listof ExprC)]) #:transparent)        ;function call
(struct IdC     ([id : Symbol]) #:transparent)                                ;variable
(struct Ifleq0C ([c : ExprC] [y : ExprC] [n : ExprC]) #:transparent)          ;simple conditional
;functions
(struct FundefC ([name : Symbol] [arg : (Listof Symbol)] [body : ExprC]) #:transparent)



;TOP-INTERP
;in: list of oazo3 syntax functions fun-sexps
;out: the evaluation of main function in fun-sexps
(: top-interp (Sexp -> Real))
(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps)))



;INTERP-FNS
;in: list of FundefC funcs
;out: evaluation of funtions as a Real
(define (interp-fns [funs : (Listof FundefC)]) : Real
  (interp (FundefC-body (get-fundef 'main funs 0)) funs))



;get-fundef
;in: a symbol n and list of FundefC fds and the number of arguments a function should take
;out: the function in fds with name n
(define (get-fundef [n : Symbol] [fds : (Listof FundefC)] [num-args : Real]) : FundefC
  (cond
    [(empty? fds) (error 'get-fundef "OAZO4 reference to undefined function")]
    [(cons? fds) (cond
                   [(and
                     (eq? n (FundefC-name (first fds)))
                     (eq? num-args (length (FundefC-arg (first fds))))) (first fds)]
                   [else (get-fundef n (rest fds) num-args)])]))




;INTERP
;in: ExprC exp, list of FundefC lst
;out: evaluation of exp as a Real
(define (interp [exp : ExprC] [funs : (Listof FundefC)]) : Real
  (match exp
    [(NumC n) n]
    [(BinopC '+ l r) (+ (interp l funs) (interp r funs))]
    [(BinopC '- l r)  (- (interp l funs) (interp r funs))]
    [(BinopC '* l r) (* (interp l funs) (interp r funs))]
    [(BinopC '/ l r)  (cond [(not (eq? (interp r funs) 0)) (/ (interp l funs) (interp r funs))]
                            [else (error 'interp "OAZO4 div by 0")])] 
    [(Ifleq0C c y n) (cond [(<= (interp c funs) 0) (interp y funs)]
                           [else (interp n funs)])]

    [(AppC name args) (define fd (get-fundef name funs (length args)))
                      (interp (subst-multi
                               (for/list ([item (in-list args)])
                                 (NumC (interp item funs)))
                               (FundefC-arg fd)
                               (FundefC-body fd)) 
                              funs)]
    [(IdC _) (error 'interp "OAZO4 wrong arity")])) 



;SUBST-MULTI
;in: list of ExprC what, list of Symbol for, and ExprC in
;out: 
(define (subst-multi [what : (Listof ExprC)] [for : (Listof Symbol)] [in : ExprC]) : ExprC
  (match what
    ['() in]
    [(cons f r) (subst-multi r (rest for) (subst f (first for) in))]))



;SUBST
;in: ExprC what, Symbol for, and ExprC in
;out: 
(define (subst [what : ExprC] [for : Symbol] [in : ExprC]) : ExprC
  (match in
    [(NumC n) in]
    [(IdC s) (cond
               [(symbol=? s for) what]
               [else in])]
    [(AppC name args) (AppC name (for/list ([item (in-list args)])
                                   (subst what for item)))]
    [(BinopC '+ l r) (BinopC '+ (subst what for l)
                             (subst what for r))]
    [(BinopC '* l r) (BinopC '* (subst what for l)
                             (subst what for r))]
    [(BinopC '- l r) (BinopC '- (subst what for l)
                             (subst what for r))]
    [(BinopC '/ l r) (BinopC '/ (subst what for l)
                             (subst what for r))]
    [(Ifleq0C c y n) (Ifleq0C (subst what for c) (subst what for y)
                              (subst what for n))]))



;PARSE-PROG
;in: s-expression code
;out: the parsed program (list of FundefC)
(define (parse-prog [code : Sexp]) : (Listof FundefC)
  (match code
    ['() '()]
    [(cons f r) (cons (parse-fundef f) (parse-prog r))]))



;PARSE-FUNDEF
;in: s-expression code
;out: the parsed FundefC representation of code
(define (parse-fundef [code : Sexp]) : FundefC
  (match code
    [(list 'func (list (? symbol? (? is-allowed? name)) ...) ': expr)
     (FundefC (cast (first name) Symbol) (cast (rest name) (Listof Symbol)) (parse expr))]
    [other (error 'parse-fundef "OAZO4 syntax error.")]))  


;PARSE
;in: s-expression code
;out: the parsed ExprC representation of code
(define (parse [code : Sexp]) : ExprC
  (match code
    [(? real? n)   (NumC n)]
    [(? symbol? s) (cond [(is-allowed? s) (IdC s)]
                         [else (error 'parse "OAZO4 keyword error: ~e" s)])]
    [(list '+ l r) (BinopC '+ (parse l) (parse r))]
    [(list '- l r) (BinopC '- (parse l) (parse r))]
    [(list '* l r) (BinopC '* (parse l) (parse r))]
    [(list '/ l r) (BinopC '/ (parse l) (parse r))]
    [(list 'ifleq0? f s r) (Ifleq0C (parse f) (parse s) (parse r))]
    [(cons (? symbol? name) r) (cond [(is-allowed? name) (AppC name (for/list ([item (in-list r)]) 
                                                                      (parse (cast item Sexp))))] 
                                     [else (error 'parse "OAZO4 keyword error")])]
    [other (error 'parse "OAZO4 syntax error in ~e" other)]))



;IS-ALLOWED?
;takes in a symbol
;returns false if it is a keyword, otherwise true
(define (is-allowed? [s : Symbol]) : Boolean
  (match s
    ['+ #f]
    ['- #f]
    ['* #f]
    ['/ #f]
    ['ifleq0? #f]
    ['func #f]
    [': #f]
    [else #t]))


;TESTS 
;; parse
(check-equal? (parse '{/ {* {- {+ x 10} 10} 10} 10})
              (BinopC '/ (BinopC '* (BinopC '- (BinopC '+ (IdC 'x) (NumC 10)) (NumC 10)) (NumC 10)) (NumC 10)))
(check-equal? (parse '{ifleq0? {- x 7} yes no}) (Ifleq0C (BinopC '- (IdC 'x) (NumC 7)) (IdC 'yes) (IdC 'no)))
(check-equal? (parse '{f}) (AppC 'f '()))
(check-equal? (parse '{f 10}) (AppC 'f (list (NumC 10))))
(check-equal? (parse '{f 8 a 3 7 b}) (AppC 'f (list (NumC 8) (IdC 'a) (NumC 3) (NumC 7) (IdC 'b))))
(check-equal? (parse '{f {+ 1 2} {- 1 2}}) (AppC 'f (list (BinopC '+ (NumC 1) (NumC 2)) (BinopC '- (NumC 1) (NumC 2)))))
(check-equal? (parse '{f {+ 1 2} {- 1 2} {* 1 2}})
              (AppC 'f
                    (list (BinopC '+ (NumC 1) (NumC 2)) (BinopC '- (NumC 1) (NumC 2)) (BinopC '* (NumC 1) (NumC 2)))))
(check-exn #rx"OAZO4 syntax" (lambda () (parse '{1 + 2})))
(check-exn #rx"OAZO4 keyword" (lambda () (parse '{+ 1 2 3})))
(check-exn #rx"OAZO4 keyword" (lambda () (parse '{+ + 2})))

;; parse-fundef
(check-equal? (parse-fundef '{func {f x y} : {+ x y}}) (FundefC 'f '(x y) (BinopC '+ (IdC 'x) (IdC 'y))))
(check-equal? (parse-fundef '{func {f } : {+ 1 2}}) (FundefC 'f '() (BinopC '+ (NumC '1) (NumC '2))))
(check-exn #rx"OAZO4" (lambda () (parse-fundef '{+ + 2})))

;; interp
(check-equal? (interp (AppC 'f {list (NumC 1) (NumC 2)}) (list (FundefC 'f '(x y)
                                                                        (BinopC '+ (IdC 'x) (IdC 'y))))) 3)
(check-equal? (interp (Ifleq0C (AppC 'f {list (NumC 1) (NumC 2)}) (NumC 1) (NumC 2))
                      (list (FundefC 'f '(x y) (BinopC '+ (IdC 'x) (IdC 'y))))) 2)
(check-equal? (interp (Ifleq0C (AppC 'f {list (NumC 1) (NumC 2)}) (NumC 1) (NumC 2))
                      (list (FundefC 'f '(x y) (BinopC '- (IdC 'x) (IdC 'y))))) 1)
(check-exn #rx"OAZO4 div" (lambda () (interp (BinopC '/ (NumC 1) (NumC 0)) '())))
(check-exn #rx"OAZO4 wrong arity" (lambda () (interp (IdC 'r) '())))


;; parse-prog
(define test0 '{{func {main} : {+ 1 2}}})
(define test1 '{{func {f x} : {+ x 14}}
                {func {main} : {f 2}}})
(define test2 '{{func {adder y} : {+ y 2}}
                {func {suber x} : {adder x}}
                {func {main} : {suber 10}}})

;; subst
(check-equal? (subst (NumC 0) 'x (BinopC '+ (IdC 'x) (IdC 'y))) (BinopC '+ (NumC 0) (IdC 'y)))
(check-equal? (subst (NumC 0) 'x (BinopC '+ (IdC 'x) (IdC 'y))) (BinopC '+ (NumC 0) (IdC 'y)))
(check-equal? (top-interp '{{func {f x y} : {+ x y}}
                            {func {main} : {f 1 2}}}) 3)
(check-equal? (top-interp '{{func {f} : 5}
                            {func {main} : {+ {f} {f}}}}) 10)
(check-equal? (top-interp '{{func {f x y} : {/ {- {* x y} 2} 2}}
                            {func {main} : {f 2 2}}}) 1)

;; is allowed
(check-equal? (is-allowed? '+) #f)
(check-equal? (is-allowed? '-) #f)
(check-equal? (is-allowed? '/) #f)
(check-equal? (is-allowed? '*) #f)
(check-equal? (is-allowed? ':) #f)
(check-equal? (is-allowed? 'ifleq0?) #f)
(check-equal? (is-allowed? 'func) #f)
(check-equal? (is-allowed? 'sym) #t)


;get-fundef
(define fun-list (list (FundefC 'f '(x y) (BinopC '+ (IdC 'x) (IdC 'y)))
                       (FundefC 'g '(c b) (BinopC '+ (IdC 'c) (IdC 'b)))))
(check-equal? (get-fundef 'f fun-list 2) (FundefC 'f '(x y) (BinopC '+ (IdC 'x) (IdC 'y))))
(check-exn #rx"OAZO4 reference" (lambda () (get-fundef 't fun-list 2)))

;subst
(check-equal? (subst (NumC 0) 'x (BinopC '+ (IdC 'x) (IdC 'y))) (BinopC '+ (NumC 0) (IdC 'y)))
(check-equal? (subst (NumC 0) 'x (AppC 'f (list (IdC 'x) (NumC 4)))) (AppC 'f (list (NumC 0) (NumC 4))))
(check-equal? (subst (NumC 0) 'x (Ifleq0C (IdC 'x) (NumC 5) (NumC 6))) (Ifleq0C (NumC 0) (NumC 5) (NumC 6)))


