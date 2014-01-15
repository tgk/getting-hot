(ns part-deux
  (:use propaganda.system
        propaganda.values)
  (:require [propaganda.generic-operators :as go]
            [clojure.set :refer [intersection union difference]]))

(defn generic-set-operator
  [op]
  (doto (go/generic-operator op)
    (go/assign-operation
     (fn [s v] (into #{} (for [elm s] (op elm v))))
     set? any?)
    (go/assign-operation
     (fn [v s] (into #{} (for [elm s] (op v elm))))
     any? set?)
    (go/assign-operation
     (fn [s1 s2] (into #{} (for [e1 s1 e2 s2] (op e1 e2))))
     set? set?)))

(def plus (generic-set-operator +))
(def minus (generic-set-operator -))
(def multiply (generic-set-operator *))
(def divide (generic-set-operator /))

(plus 1 2)
;; => 3
(plus #{1 2 3} 4)
;; => #{5 6 7}
(plus 4 #{1 2 3})
;; => #{5 6 7}
(plus #{1 2 3} #{1 2 3})
;; => #{2 3 4 5 6}

(def plusp (function->propagator-constructor plus))
(def minusp (function->propagator-constructor minus))
(def multiplyp (function->propagator-constructor multiply))
(def dividep (function->propagator-constructor divide))

(defn sum
  [system a b c]
  (-> system
      (plusp a b c)
      (minusp c a b)
      (minusp c b a)))

(defn prod
  [system a b c]
  (-> system
      (multiplyp a b c)
      (dividep c a b)
      (dividep c b a)))

(-> (make-system)
    (prod :a :b :c)
    (add-value :a 10)
    (add-value :b 2)
    (get-value :c))
;; => 20

(-> (make-system)
    (prod :a :b :c)
    (add-value :a 2)
    (add-value :c 84)
    (get-value :b))
;; => 42

(defn c-f-relation
  [system f c]
  (let [minus-32    (gensym "minus-32")
        five-ninths (gensym "five-ninths")
        f-minus-32  (gensym "f-minus-32")]
    (-> system
        (add-value minus-32 -32)
        (add-value five-ninths 5/9)
        (sum f minus-32 f-minus-32)
        (prod five-ninths f-minus-32 c))))

(-> (make-system)
    (c-f-relation :temp-f :temp-c)
    (add-value :temp-f 100N)
    (get-value :temp-c))
;; => 340/9

(-> (make-system)
    (c-f-relation :temp-f :temp-c)
    (add-value :temp-c 340/9)
    (get-value :temp-f))
;; => 100N

#_(-> (make-system)
    (c-f-relation :temp-f :temp-c)
    (add-value :temp-f 100N)
    (add-value :temp-c 38N)
    (get-value :temp-c))
;; throws ExceptionInfo Inconsistency

;; ------------------------------------------------------
;; But also support for sets

(defn check-intersection
  [s1 s2]
  (let [i (intersection s1 s2)]
    (if (seq i)
      (if (= 1 (count i))
        (first i)
        i)
      (contradiction
       (format "Intersection of %s and %s is empty" s1 s2)))))

(defn check-in-set
  [e s]
  (if (contains? s e)
    e
    (contradiction
     (format "%s is not in %s" e s))))

(defn extend-merge
  [merge]
  (doto merge
    (go/assign-operation
     (fn [content increment]
       (check-in-set increment content))
     set? any?)
    (go/assign-operation
     (fn [content increment]
       (check-in-set content increment))
     any? set?)
    (go/assign-operation
     check-intersection
     set? set?)))

(let [my-merge (doto (default-merge)
                 extend-merge)
      my-contradictory? (default-contradictory?)]
  (-> (make-system my-merge my-contradictory?)
      (c-f-relation :temp-f :temp-c)
      (add-value :temp-f 100N)
      (get-value :temp-c)))
;; => 340/9

(let [my-merge (doto (default-merge)
                 extend-merge)
      my-contradictory? (default-contradictory?)]
  (-> (make-system my-merge my-contradictory?)
      (c-f-relation :temp-f :temp-c)
      (add-value :temp-f #{100N 200N})
      (get-value :temp-c)))
;; => #{280/3 340/9}

(let [my-merge (doto (default-merge)
                 extend-merge)
      my-contradictory? (default-contradictory?)]
  (-> (make-system my-merge my-contradictory?)
      (c-f-relation :temp-f :temp-c)
      (add-value :temp-f #{100N 200N})
      (add-value :temp-c #{110/4 280/3})
      (get-value :temp-c)))
;; => 280/3
