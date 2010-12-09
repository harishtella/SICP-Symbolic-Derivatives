; Some expiriments and exercises for 
; Lecture 3B of SICP


(ns deriv)

(defn variable? 
  [x] 
  (symbol? x))

(defn same-variable?
  [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2))) 

(defn sum? 
  [x] 
  (and (seq? x) (= (first x) '+)))

(defn make-sum
  [a1 a2] 
  (cond 
    (= a1 0) a2 
    (= a2 0) a1
    (and (number? a1) (number? a2)) (+ a1 a2)
    :else (list '+ a1 a2)))

(defn make-product
  [a1 a2] 
  (cond 
    (or (= a1 0) (= a2 0)) 0
    (= a1 1) a2 
    (= a2 1) a1
    (and (number? a1) (number? a2)) (* a1 a2)
    :else (list '* a1 a2)))

(defn product?
  [x] 
  (and (seq? x) (= (first x) '*)))

(defn addend 
  [[op addend & augend]]
    addend)

(defn augend
  [[op addend & augend]] 
  (if (next augend)
    (conj augend '+)
    (first augend)))

(defn multiplier 
  [[op multiplier & multiplicand]]
    multiplier)

(defn multiplicand
  [[op multiplier & multiplicand]]
  (if (next multiplicand)
    (conj multiplicand '*)
    (first multiplicand)))


(defn exponentiation? 
  [x]
  (and (seq? x) (= (first x) '**)))

(defn base 
  [[op base exponent]] 
  base)

(defn exponent
  [[op base exponent]] 
  exponent)

(defn make-exponentiation
  [base exponent]
  (cond 
    (= exponent 0) 1
    (= exponent 1) base
    :else (list '** base exponent)))
 


(defn deriv
  [exp var] 
  (cond 
    (number? exp) 0
    (variable? exp) (if
                      (same-variable? exp var)
                      1
                      0)
    (sum? exp) (make-sum
                 (deriv (addend exp) var)
                 (deriv (augend exp) var))
    (product? exp) (make-sum
                     (make-product (multiplier exp)
                                   (deriv (multiplicand exp) var))
                     (make-product (multiplicand exp)
                                   (deriv (multiplier exp) var)))
    (exponentiation? exp) (make-product 
                            (deriv (base exp) var)
                            (make-product 
                              (exponent exp) 
                              (make-exponentiation 
                                (base exp)
                                (- (exponent exp) 1))))
    :else
      (throw (Exception. (str "unknown expression type -- DERIV " exp)))))

