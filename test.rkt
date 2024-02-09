#lang typed/racket
(require typed/rackunit)


(define-type ExprC (U NumC BinopC IdC Ifleq0C AppC))
(struct NumC    ([n : Real]) #:transparent)                                   ;numbers
(struct BinopC  ([operation : Symbol] [l : ExprC] [r : ExprC]) #:transparent) ;+-*/
(struct AppC    ([fun : Symbol] [arg : (Listof ExprC)]) #:transparent)                 ;function call
(struct IdC     ([id : Symbol]) #:transparent)                                ;variable
(struct Ifleq0C ([c : ExprC] [y : ExprC] [n : ExprC]) #:transparent)          ;simple conditional
(struct FundefC ([name : Symbol] [arg : (Listof Symbol)] [body : ExprC]) #:transparent)


(define (parse [code : Sexp]) : ExprC
  (match code
    [(? real? n)   (NumC n)]
    [(? symbol? s) (cond [(is-allowed? s) (IdC s)]
                         [else (error 'parse "OAZO4 key word ~e identified" s)])]
    [(list '+ l r) (BinopC '+ (parse l) (parse r))] 
    [(list '- l r) (BinopC '- (parse l) (parse r))]
    [(list '* l r) (BinopC '* (parse l) (parse r))]
    [(list '/ l r) (BinopC '/ (parse l) (parse r))]
    #;[(list s l r) (error 'parse "OAZO4 ~e operation not supported" s)] ;; Need to fix
    [(list 'ifleq0? f s r) (Ifleq0C (parse f) (parse s) (parse r))]
    [(cons (? symbol? name) r) (cond [(is-allowed? name) (AppC name (for/list ([item (in-list r)]) 
                                                                      (parse (cast item Sexp))))] 
                                     [else (error 'parse "OAZO4 Keyword error")])]
    [other (error 'parse "OAZO4 syntax error in ~e" other)]))  
 

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


(define (subst-multi [what : (Listof ExprC)] [for : (Listof Symbol)] [in : ExprC]) : ExprC
  (match what
    ['() in]
    [(cons f r) (subst-multi r (rest for) (subst f (first for) in))])) 


;get-fundef
;in: a symbol n and list of FundefC fds
;out: the function in fds with name n
(define (get-fundef [n : Symbol] [fds : (Listof FundefC)]) : FundefC
  (cond
    [(empty? fds) (error 'get-fundef "OAZO4 reference to undefined function")] 
    [(cons? fds) (cond
                   [(equal? n (FundefC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))


(define (parse-fundef [code : Sexp]) : FundefC
    (match code
      [(list 'func (list (? symbol? (? is-allowed? name)) ...) ': expr)
       (FundefC (cast (first name) Symbol) (cast (rest name) (Listof Symbol)) (parse expr))]
      [other (error 'parse-fundef "OAZO4 syntax")]))  



;parse-prog
;in: s-expression code 
;out: the parsed program (list of FundefC)
(define (parse-prog [code : Sexp]) : (Listof FundefC) 
    (match code
      ['() '()]
      [(cons f r) (cons (parse-fundef f) (parse-prog r))]))

(define (interp-fns [funs : (Listof FundefC)]) : Real
  (interp (FundefC-body (get-fundef 'main funs)) funs)) 

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

    [(AppC name args) (define fd (get-fundef name funs))
                      (interp (subst-multi (for/list ([item (in-list args)])
                                             (NumC (interp item funs)))
                                           (FundefC-arg fd)
                                           (FundefC-body fd)) 
                              funs)]
    [(IdC _) (error 'interp "OAZO4 shouldn't get here")])) 


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

(define (is-allowed-params? [s : (Listof Symbol)]) : Boolean
  (match s
    ['() #t]
    [(cons f r) (cond[(is-allowed? f) (is-allowed-params? r)]
                     [else #f])]))

;top-interp 
;in: list of OAZO4 syntax functions fun-sexps
;out: the evaluation of main function in fun-sexps
(define (top-interp [fun-sexps : Sexp]) : Real
    (interp-fns (parse-prog fun-sexps)))

;TESTS 

;parse
(check-equal? (parse '{/ {* {- {+ x 10} 10} 10} 10}) (BinopC '/ (BinopC '* (BinopC '- (BinopC '+ (IdC 'x) (NumC 10)) (NumC 10)) (NumC 10)) (NumC 10)))
(check-equal? (parse '{ifleq0? {- x 7} yes no}) (Ifleq0C (BinopC '- (IdC 'x) (NumC 7)) (IdC 'yes) (IdC 'no)))
(check-equal? (parse '{f}) (AppC 'f '()))
(check-equal? (parse '{f 10}) (AppC 'f (list (NumC 10))))
(check-equal? (parse '{f 8 a 3 7 b}) (AppC 'f (list (NumC 8) (IdC 'a) (NumC 3) (NumC 7) (IdC 'b))))
(check-equal? (parse '{f {+ 1 2} {- 1 2}}) (AppC 'f (list (BinopC '+ (NumC 1) (NumC 2)) (BinopC '- (NumC 1) (NumC 2)))))
(check-equal? (parse '{f {+ 1 2} {- 1 2} {* 1 2}}) (AppC 'f (list (BinopC '+ (NumC 1) (NumC 2)) (BinopC '- (NumC 1) (NumC 2)) (BinopC '* (NumC 1) (NumC 2)))))
(check-exn #rx"OAZO4 syntax" (lambda () (parse '{1 + 2})))
(check-exn #rx"OAZO4" (lambda () (parse '{+ 1 2 3})))
(check-exn #rx"OAZO4" (lambda () (parse '{+ + 2})))


(check-equal? (parse '{f {+ 1 2} {- 1 2} {* 3 3}}) (AppC 'f (list (BinopC '+ (NumC 1) (NumC 2)) (BinopC '- (NumC 1) (NumC 2)) (BinopC '* (NumC 3) (NumC 3)))))
(check-equal? (parse '{f 1 {/ 6 3} {ifleq0? 3 3 3}}) (AppC 'f (list (NumC 1) (BinopC '/ (NumC 6) (NumC 3)) (Ifleq0C (NumC 3) (NumC 3) (NumC 3)))))

;; parse-fundef
(check-equal? (parse-fundef '{func {f x y} : {+ x y}}) (FundefC 'f '(x y) (BinopC '+ (IdC 'x) (IdC 'y))))
(check-equal? (parse-fundef '{func {f} : {+ 1 2}}) (FundefC 'f '() (BinopC '+ (NumC '1) (NumC '2))))
(check-equal? (parse-fundef '{func {f x} : {+ x 2}}) (FundefC 'f '(x) (BinopC '+ (IdC 'x) (NumC '2))))
(check-exn #rx"OAZO4" (lambda () (parse-fundef '{+ + 2})))

;; interp
(check-equal? (interp (AppC 'f {list (NumC 1) (NumC 2)}) (list (FundefC 'f '(x y) (BinopC '+ (IdC 'x) (IdC 'y))))) 3) 

;; top-interp
(define test0 '{{func {main init} : {+ 1 2}}}) 
(define test1 '{{func {f x} : {+ x 14}}
                {func {main init} : {f 2}}})
(check-equal? (top-interp test0) 3)
(check-equal? (top-interp test1) 16)

(check-equal? (interp-fns
       (parse-prog '{{func {f x y} : {+ x y}}
                     {func {main} : {f 1 2}}}))
      3)

(check-equal? (interp-fns
        (parse-prog '{{func {f} : 5}
                      {func {main} : {+ {f} {f}}}})) 
       10)




#;(define test0 '{{func {main init} : {+ 1 2}}})
(define test0-parsed (list (FundefC 'main '(init) (BinopC '+ (NumC 1) (NumC 2)))))
#;(define test1 '{{func {f x} : {+ x 14}}
                {func {main init} : {f 2}}})
(define test1-parsed (list
                      (FundefC 'f '(x) (BinopC '+ (IdC 'x) (NumC 14)))
                      (FundefC 'main '() (AppC 'f (list (NumC 2))))))
(define test2 '{{func {adder y} : {+ y 2}}
                {func {suber x} : {adder x}}
                {func {main} : {suber 10}}}) ;; FAILING
(define test2-parsed (list
                      (FundefC 'adder '(y) (BinopC '+ (IdC 'y) (NumC 2)))
                      (FundefC 'suber '(x) (AppC 'adder (list (IdC 'x))))
                      (FundefC 'main '(param) (AppC 'suber (list (NumC 10))))))

(define testSub '{{func {adder y} : {- y 2}}
                  {func {suber x} : {adder x}}
                  {func {main } : {suber 10}}})

(define testMult '{{func {adder y} : {* y 2}}
                   {func {suber x} : {adder x}}
                   {func {main } : {suber 10}}})

(define testDiv '{{func {adder y} : {/ y 2}}
                  {func {suber x} : {adder x}}
                  {func {main } : {suber 10}}})

;top-interp
#;((check-equal? (top-interp test0) 3)
(check-equal? (top-interp test1) 16) 
(check-equal? (top-interp test2) 12)  ;; FAILING
(check-equal? (top-interp testSub) 8)
(check-equal? (top-interp testMult) 20)
(check-equal? (top-interp testDiv) 5)
(check-equal? (top-interp (quote ((func (main init) : (ifleq0? (* 3 1) 3 (+ 2 9)))))) 11)
(check-equal? (top-interp (quote ((func (main init) : (+ (f 13) (f 0))) (func (f qq) : (ifleq0? qq qq (+ qq 1)))))) 14)
(check-exn #rx"div" (lambda()
                      (top-interp '((func (ignoreit x) : (+ 3 4)) (func (main init) : (ignoreit (/ 1 (+ 0 0)))))))) 
)

;interp-fns
(check-equal? (interp-fns (parse-prog test0)) 3)
(check-equal? (interp-fns test1-parsed) 16)
(check-equal? (interp-fns test2-parsed) 12) 

;get-fundef
(check-equal? (get-fundef 'main test0-parsed) (FundefC 'main '{init} (BinopC '+ (NumC 1) (NumC 2))))
(check-equal? (get-fundef 'main test1-parsed) (FundefC 'main '{init} (AppC 'f (list (NumC 2)))))
(check-equal? (get-fundef 'main test2-parsed) (FundefC 'main '{param} (AppC 'suber (list (NumC 10))))) 
(check-equal? (get-fundef 'adder test2-parsed) (FundefC 'adder '{y} (BinopC '+ (IdC 'y) (NumC 2))))
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

;parse-prog
(check-equal? (parse-prog test0) test0-parsed)
(check-equal? (parse-prog test1) test1-parsed)
(check-equal? (parse-prog test2) test2-parsed)
(check-exn #rx"syntax error" (lambda () (parse-prog '(+ 2))))

;parse-fundef
(check-equal? (parse-fundef '{func {adder y} : {+ y 2}}) (FundefC 'adder '{y} (BinopC '+ (IdC 'y) (NumC 2))))
(check-equal? (parse-fundef '{func {main param} : {suber 10}}) (FundefC 'main '{param} (AppC 'suber (list (NumC 10)))))
(check-exn #rx"syntax error" (lambda () (parse-fundef '{notafunc name param : {+ 1 2}})))
(check-exn #rx"syntax error" (lambda () (parse-fundef '(+ 2))))

;parse
(check-equal? (parse 1) (NumC 1))
(check-equal? (parse 'y) (IdC 'y))
(check-equal? (parse '{+ 1 2}) (BinopC '+ (NumC 1) (NumC 2)))
(check-equal? (parse '{- 1 2}) (BinopC '- (NumC 1) (NumC 2)))
(check-equal? (parse '{* 1 2}) (BinopC '* (NumC 1) (NumC 2)))
(check-equal? (parse '{/ 1 2}) (BinopC '/ (NumC 1) (NumC 2)))
(check-equal? (parse '{f 10}) (AppC 'f (list (NumC 10))))
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
