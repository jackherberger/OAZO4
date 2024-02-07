#lang typed/racket
(require typed/rackunit)


;Arith - language definition
;expressions
(define-type ExprC (U NumC BinopC IdC Ifleq0C AppC))
(struct NumC    ([n : Real]) #:transparent)                                   ;numbers
(struct BinopC  ([operation : Symbol] [l : ExprC] [r : ExprC]) #:transparent) ;+-*/
(struct AppC    ([fun : Symbol] [arg : (Listof ExprC)]) #:transparent)                 ;function call
(struct IdC     ([id : Symbol]) #:transparent)                                ;variable
(struct Ifleq0C ([c : ExprC] [y : ExprC] [n : ExprC]) #:transparent)          ;simple conditional
;functions
(struct FundefC ([name : Symbol] [arg : Symbol] [body : ExprC]) #:transparent)

;top-interp 
;in: list of oazo3 syntax functions fun-sexps
;out: the evaluation of main function in fun-sexps
(define (top-interp [fun-sexps : Sexp]) : Real
  (interp-fns (parse-prog fun-sexps)))

;interp-fns
;in: list of FundefC funcs
;out: evaluation of funtions as a Real
(define (interp-fns [funs : (Listof FundefC)]) : Real
  (define main-init (get-fundef 'main funs))
  (define main-zero (FundefC 'main (FundefC-arg main-init) (subst (NumC 0) 'init (FundefC-body main-init))))
  (interp (FundefC-body main-zero) funs))

;get-fundef
;in: a symbol n and list of FundefC fds
;out: the function in fds with name n
(define (get-fundef [n : Symbol] [fds : (Listof FundefC)]) : FundefC
  (cond
    [(empty? fds) (error 'get-fundef "OAZO3 reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (FundefC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))

;interp
;in: ExprC exp, list of FundefC lst
;out: evaluation of exp as a Real
(define (interp [exp : ExprC] [funs : (Listof FundefC)]) : Real
  (match exp
    [(NumC n) n]
    [(BinopC '+ l r) (+ (interp l funs) (interp r funs))]
    [(BinopC '- l r)  (- (interp l funs) (interp r funs))]
    [(BinopC '* l r) (* (interp l funs) (interp r funs))]
    [(BinopC '/ l r)  (cond [(not (eq? (interp r funs) 0)) (/ (interp l funs) (interp r funs))]
                            [else (error 'interp "OAZO3 div by 0")])] 
    [(Ifleq0C c y n) (cond [(<= (interp c funs) 0) (interp y funs)]
                           [else (interp n funs)])]
    [(AppC f a) (cond [(eq? (rest a) '()) (error 'interp "OAZO3 args not 1")]
                      [else (define fd (get-fundef f funs))
                            (interp (subst (NumC (interp a funs))
                                           (FundefC-arg fd)
                                           (FundefC-body fd))
                                    funs)])]
    [(IdC _) (error 'interp "OAZO3 shouldn't get here")]))





;subst
;in: ExprC what, Symbol for, and ExprC in
;out: ExprC in with all for replaced by what
(define (subst [what : ExprC] [for : Symbol] [in : ExprC]) : ExprC
  (match in
    [(NumC n) in]
    [(IdC s) (cond
               [(symbol=? s for) what]
               [else in])]
    [(AppC f a) (AppC f (subst what for a))]
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

;parse-prog
;in: s-expression code
;out: the parsed program (list of FundefC)
(define (parse-prog [code : Sexp]) : (Listof FundefC)
  (match code
    ['() '()]
    [(cons f r) (cons (parse-fundef f) (parse-prog r))]))

;parse-fundef
;in: s-expression code
;out: the parsed FundefC representation of code
(define (parse-fundef [code : Sexp]) : FundefC
  (match code
    [(list 'func (list (? symbol? n) (? symbol? p)) ': e)
     (cond
       [(and (is-allowed? n) (is-allowed? p)) (FundefC n p (parse e))]
       [else (error 'parse "OAZO3 key word ~e videntified" n)])]
    [other (error 'parse-fundef "OAZO3 syntax error in ~e" other)]))

;parse
;in: s-expression code
;out: the parsed ExprC representation of code
(define (parse [code : Sexp]) : ExprC
  (match code
    [(? real? n)   (NumC n)]
    [(? symbol? s) (cond [(is-allowed? s) (IdC s)]
                         [else (error 'parse "OAZO3 key word ~e identified" s)])]
    [(list '+ l r) (BinopC '+ (parse l) (parse r))]
    [(list '- l r) (BinopC '- (parse l) (parse r))]
    [(list '* l r) (BinopC '* (parse l) (parse r))]
    [(list '/ l r) (BinopC '/ (parse l) (parse r))]
    [(list s l r) (error 'parse "OAZO3 ~e operation not supported" s)]
    [(list 'ifleq0? f s r) (Ifleq0C (parse f) (parse s) (parse r))]
    [(list (? symbol? name) param) (cond
                                     [(is-allowed? name) (AppC name (parse param))]
                                     [else (error 'parse "OAZO3 syntax error in ~e" name)])]
    [other (error 'parse "OAZO3 syntax error in ~e" other)]))


(define (parse-params [params : (Listof Sexp)]) )
;is-allowed?
;takes in a symbol s
;returns false if s is a keyword, otherwise true
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





;;;; TESTING

;function definitions for a runable program
(define test0 '{{func {main init} : {+ 1 2}}})
(define test0-parsed (list (FundefC 'main 'init (BinopC '+ (NumC 1) (NumC 2)))))
(define test1 '{{func {f x} : {+ x 14}}
                {func {main init} : {f 2}}})
(define test1-parsed (list
                      (FundefC 'f 'x (BinopC '+ (IdC 'x) (NumC 14)))
                      (FundefC 'main 'init (AppC 'f (NumC 2)))))
(define test2 '{{func {adder y} : {+ y 2}}
                {func {suber x} : {adder x}}
                {func {main param} : {suber 10}}})
(define test2-parsed (list
                      (FundefC 'adder 'y (BinopC '+ (IdC 'y) (NumC 2)))
                      (FundefC 'suber 'x (AppC 'adder (IdC 'x)))
                      (FundefC 'main 'param (AppC 'suber (NumC 10)))))
(define testSub '{{func {adder y} : {- y 2}}
                  {func {suber x} : {adder x}}
                  {func {main param} : {suber 10}}})
(define testMult '{{func {adder y} : {* y 2}}
                   {func {suber x} : {adder x}}
                   {func {main param} : {suber 10}}})
(define testDiv '{{func {adder y} : {/ y 2}}
                  {func {suber x} : {adder x}}
                  {func {main param} : {suber 10}}})

;top-interp
(check-equal? (top-interp test0) 3)
(check-equal? (top-interp test1) 16)
(check-equal? (top-interp test2) 12)
(check-equal? (top-interp testSub) 8)
(check-equal? (top-interp testMult) 20)
(check-equal? (top-interp testDiv) 5)
(check-equal? (top-interp (quote ((func (main init) : (ifleq0? (* 3 1) 3 (+ 2 9)))))) 11)
(check-equal? (top-interp (quote ((func (main init) : (+ (f 13) (f 0))) (func (f qq) : (ifleq0? qq qq (+ qq 1)))))) 14)
(check-exn #rx"div" (lambda()
                      (top-interp '((func (ignoreit x) : (+ 3 4)) (func (main init) : (ignoreit (/ 1 (+ 0 0)))))))) 

;interp-fns
(check-equal? (interp-fns (parse-prog test0)) 3)
(check-equal? (interp-fns test1-parsed) 16)
(check-equal? (interp-fns test2-parsed) 12)

;get-fundef
(check-equal? (get-fundef 'main test0-parsed) (FundefC 'main 'init (BinopC '+ (NumC 1) (NumC 2))))
(check-equal? (get-fundef 'main test1-parsed) (FundefC 'main 'init (AppC 'f (NumC 2))))
(check-equal? (get-fundef 'main test2-parsed) (FundefC 'main 'param (AppC 'suber (NumC 10))))
(check-equal? (get-fundef 'adder test2-parsed) (FundefC 'adder 'y (BinopC '+ (IdC 'y) (NumC 2))))
(check-exn #rx"undefined" (lambda () (get-fundef 'fakename '())))

;interp
(define tester (parse-prog'{{func {main init} : {- 1 2}}}))
(define sub (BinopC '- (NumC 3) (NumC 2))) 
(check-equal? (interp sub tester) 1)
(define mult (BinopC '* (NumC 3) (NumC 2))) 
(check-equal? (interp mult tester) 6)
(define div (BinopC '/ (NumC 6) (NumC 2))) 
(check-equal? (interp div tester) 3)
(define leq_y (Ifleq0C (NumC -4) (NumC 6) (NumC 2))) 
(check-equal? (interp leq_y tester) 6)
(define leq_n (Ifleq0C (NumC 4) (NumC 6) (NumC 2))) 
(check-equal? (interp leq_n tester) 2)
(check-exn #rx"OAZO3 shouldn't get here" (lambda () (interp (IdC 'a) tester)))
  
;subst
(check-equal? (subst (NumC 0) 'x (BinopC '+ (IdC 'x) (IdC 'y))) (BinopC '+ (NumC 0) (IdC 'y)))
(check-equal? (subst (NumC 0) 'x (BinopC '+ (IdC 'x) (NumC 0))) (BinopC '+ (NumC 0) (NumC 0)))
(check-equal? (subst (NumC 0) 'g (BinopC '+ (IdC 'x) (IdC 'y))) (BinopC '+ (IdC 'x) (IdC 'y)))

;parse-prog
(check-equal? (parse-prog test0) test0-parsed)
(check-equal? (parse-prog test1) test1-parsed)
(check-equal? (parse-prog test2) test2-parsed)
(check-exn #rx"syntax error" (lambda () (parse-prog '(+ 2))))

;parse-fundef
(check-equal? (parse-fundef '{func {adder y} : {+ y 2}}) (FundefC 'adder 'y (BinopC '+ (IdC 'y) (NumC 2))))
(check-equal? (parse-fundef '{func {main param} : {suber 10}}) (FundefC 'main 'param (AppC 'suber (NumC 10))))
(check-exn #rx"syntax error" (lambda () (parse-fundef '{notafunc name param : {+ 1 2}})))
(check-exn #rx"syntax error" (lambda () (parse-fundef '(+ 2))))

;parse
(check-equal? (parse 1) (NumC 1))
(check-equal? (parse 'y) (IdC 'y))
(check-equal? (parse '{+ 1 2}) (BinopC '+ (NumC 1) (NumC 2)))
(check-equal? (parse '{- 1 2}) (BinopC '- (NumC 1) (NumC 2)))
(check-equal? (parse '{* 1 2}) (BinopC '* (NumC 1) (NumC 2)))
(check-equal? (parse '{/ 1 2}) (BinopC '/ (NumC 1) (NumC 2)))
(check-equal? (parse '{f 10}) (AppC 'f (NumC 10)))
(check-equal? (parse '{ifleq0? -1 5 3}) (Ifleq0C (NumC -1) (NumC 5) (NumC 3)))
(check-exn #rx"syntax error" (lambda () (parse '(+ 2 2 2))))
(check-exn #rx"operation" (lambda () (parse '(% 2 2))))
(check-exn #rx"key" (lambda () (parse '(+ / 2))))

;is-allowed?
(check-equal? (is-allowed? '+) #f)
(check-equal? (is-allowed? '-) #f)
(check-equal? (is-allowed? '/) #f)
(check-equal? (is-allowed? '*) #f)
(check-equal? (is-allowed? ':) #f)
(check-equal? (is-allowed? 'ifleq0?) #f)
(check-equal? (is-allowed? 'func) #f)
(check-equal? (is-allowed? 'sym) #t)
(check-exn #rx"key" (lambda () (parse-fundef '(func (+ x) : 13))))
(check-exn #rx"syntax" (lambda () (parse '(+ b))))
