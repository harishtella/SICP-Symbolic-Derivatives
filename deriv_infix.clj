; Some expiriments and exercises for 
; Lecture 3B of SICP

; Its really amazing how powerful it was to separate the derivative rules from
; the representation of the expressions. It made it so easy to change
; represenations. Really worth grokking. 

(ns deriv-infix)

(defn variable? 
  [x] 
  (symbol? x))

(defn same-variable?
  [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2))) 

(defn sum?
  [x]
  (and (seq? x)
       (loop [[term & xs] x]
         (if term
           (if (= term '+) 
             true
             (recur xs))
           false))))

(defn simp-1-elem-list
  [x]
  (if (next x)
    x
    (first x)))
    

(defn addend 
  [x]
  (loop [[term & xs] x addend-terms []]
    (if term
      (if (= term '+) 
        (simp-1-elem-list (apply list addend-terms))
        (recur xs (conj addend-terms term)))
      (throw (Exception. (str "bad sum expression -- ADDEND " x))))))

(defn augend
  [x]
  (loop [[term & xs] x addend-terms []]
    (if term
      (if (= term '+) 
        (simp-1-elem-list xs)
        (recur xs (conj addend-terms term)))
      (throw (Exception. (str "bad sum expression -- ADDEND " x))))))

(defn make-sum
  [a1 a2] 
  (cond 
    (= a1 0) a2 
    (= a2 0) a1
    (and (number? a1) (number? a2)) (+ a1 a2)
    :else (list a1 '+ a2)))

(defn product?
  [x] 
  (and (seq? x) (= (second x) '*)))


(defn multiplier 
  [[multiplier op multiplicand]]
  multiplier)

(defn multiplicand
  [[multiplier op & multiplicand]]
  (if (next multiplicand)
    multiplicand
    (first multiplicand)))

(defn make-product
  [a1 a2] 
  (cond 
    (or (= a1 0) (= a2 0)) 0
    (= a1 1) a2 
    (= a2 1) a1
    (and (number? a1) (number? a2)) (* a1 a2)
    :else (list a1 '* a2)))


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
    :else
      (throw (Exception. (str "unknown expression type -- DERIV " exp)))))

