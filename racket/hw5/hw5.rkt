;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1
(define (racketlist->mupllist rl) (
  if (null? rl)
    (aunit)
    (apair
      (car rl)
      (racketlist->mupllist (cdr rl))
    )
  )
)

(define (mupllist->racketlist ml) (
  if (aunit? ml)
    null
    (cons (apair-e1 ml) (mupllist->racketlist (apair-e2 ml)))
  )
)

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

(define (debug-env xs)
  (if (null? xs)
    (begin (println "END") (println "") null)
    (begin
      (print (car (car xs)))
      (print " = ")
      (print (cdr (car xs)))
      (print " -> ")
      (debug-env (cdr xs)))))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and
                 (int? v1)
                 (int? v2))
             (if (> (int-num v1)
                    (int-num v2))
               (eval-under-env (ifgreater-e3 e) env)
               (eval-under-env (ifgreater-e4 e) env))
             (error "MUPL ifgreater applied to non-number"))
           )]
        [(mlet? e)
         (let ([v1 (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) v1) env))
           )]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2)
           )]
        [(aunit? e) e]
        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env)) (int 1) (int 0))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
             (apair-e1 v) 
             (error "MUPL fst applied to non-apair"))
           )]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v) 
             (apair-e2 v) 
             (error "MUPL snd applied to non-apair"))
           )]
        [(fun? e) (closure env e)]
        [(call? e)
         (let ([clo (eval-under-env (call-funexp e) env)]
               [arg (eval-under-env (call-actual e) env)])
           (if (closure? clo)
             (let* ([func (closure-fun clo)]
                    [env-with-arg (cons (cons (fun-formal func) arg) (closure-env clo))]
                    [clo-env 
                      (if (fun-nameopt func)
                        (cons (cons (fun-nameopt func) clo) env-with-arg)
                        env-with-arg)])
               (eval-under-env (fun-body func) clo-env))
            (error "MUPL call applied to non-closure"))
          )]
        [(closure? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

#| Testing recursion |#
#| (eval-exp (call (fun "add_until" "x" |#
#|                      (ifgreater (var "x") (int 0)  |#
#|                                 (add |#
#|                                   (var "x") |#
#|                                   (call (var "add_until") (add (var "x") (int -1)))) |#
#|                                 (int 0))) |#
#|                 (int 1))) |#

;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2) "CHANGE")

(define (ifeq e1 e2 e3 e4) "CHANGE")

;; Problem 4

(define mupl-map (fun "map" "mapper" (fun "_map" "list" (
                                                      ifaunit (var "list")
                                                        (aunit)
                                                        (apair
                                                          (call (var "mapper") (fst (var "list")))
                                                          (call (var "_map") (snd (var "list"))))
                                                        ))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
    (fun #f "i" (fun #f "list" (call (call mupl-map (fun #f "x" (add (var "x") (var "i")))) (var "list"))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
