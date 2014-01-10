(ns getting-hot
  (:use propaganda.system
        propaganda.values)
  (:require [propaganda.generic-operators :as go]
            [clojure.set :refer [intersection union difference]]))

;; ------------------------------------------------------
;; Simple conversion functions

(defn f->c
  [f]
  (* 5/9 (- f 32)))

(f->c 100N)
;; => 340/9 (37.8)

(defn c->f
  [c]
  (+ (* 9/5 c) 32))

(c->f (f->c 100N))
;; => 100N


;; ------------------------------------------------------
;; Defining the bijective relation

(defn c-f-relation
  [system f c]
  (-> system
      ((function->propagator-constructor f->c) f c)
      ((function->propagator-constructor c->f) c f)))

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

(-> (make-system)
    (c-f-relation :temp-f :temp-c)
    (add-value :temp-f 100N)
    (add-value :temp-c 340/9)
    (get-value :temp-c))
;; => 340/9

#_(-> (make-system)
      (c-f-relation :temp-f :temp-c)
      (add-value :temp-f 100N)
      (add-value :temp-c 38N)
      (get-value :temp-c))
;; throws ExceptionInfo Inconsistency


;; ------------------------------------------------------
;; Defining the injective relation

(defn c->cold-or-hot
  [c]
  (cond
   (< c  0) :cold
   (> c 30) :hot
   :else    nothing))

(def cold-or-hot-relation
  (function->propagator-constructor c->cold-or-hot))

(-> (make-system)
    (cold-or-hot-relation :temp-c :how-it-feels)
    (add-value :temp-c -30N)
    (get-value :how-it-feels))
;; => :cold

(-> (make-system)
    (cold-or-hot-relation :temp-c :how-it-feels)
    (c-f-relation :temp-f :temp-c)
    (add-value :temp-f 100N)
    (get-value :how-it-feels))
;; => :hot

#_(-> (make-system)
      (cold-or-hot-relation :temp-c :how-it-feels)
      (c-f-relation :temp-f :temp-c)
      (add-value :temp-f 100N)
      (add-value :how-it-feels :cold))
;; throws ExceptionInfo Inconsistency


;; ------------------------------------------------------
;; Generic operators

(let [plus (go/generic-operator +)]
  (doto plus
    (go/assign-operation concat vector? vector?))
  [(plus 1 2)
   (plus [1 2 3] [4 5])])
;; => [3 [1 2 3 4 5]]

(let [plus (go/generic-operator +)]
  (doto plus
    (go/assign-operation (partial map +) vector? vector?))
  [(plus 1 2)
   (plus [1 2 3] [4 5])])
;; => [3 (5 7)]


;; ------------------------------------------------------
;; Extending merge with sets

(defn check-intersection
  [s1 s2]
  (let [i (intersection s1 s2)]
    (if (seq i)
      (if (= 1 (count i))
        (first i)
        i)
      (contradiction
       (format "Intersection of %s and %s is empty" s1 s2)))))

(check-intersection #{:foo :bar} #{:foo :bar :baz})
;; => #{:foo :bar}

(check-intersection #{:foo :bar} #{:foo})
;; => :foo

(check-intersection #{:foo} #{:baz})
;; => #propaganda.values.Contradiction{:reason "Intersection of #{:foo} and #{:baz} is empty"}

(defn check-in-set
  [e s]
  (if (contains? s e)
    e
    (contradiction
     (format "%s is not in %s" e s))))

(check-in-set :foo #{:foo :bar})
;; => :foo

(check-in-set :foo #{:bar})
;; => #propaganda.values.Contradiction{:reason ":foo is not in #{:bar}"}

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
      (add-value :cell #{:foo :bar :baz})
      (add-value :cell #{:foo :bar})
      (get-value :cell)))
;; => #{:foo :bar}

(let [my-merge (doto (default-merge)
                 extend-merge)
      my-contradictory? (default-contradictory?)]
  (-> (make-system my-merge my-contradictory?)
      (add-value :cell #{:foo :bar})
      (add-value :cell #{:foo})
      (get-value :cell)))
;; => :foo

#_(let [my-merge (doto (default-merge)
                 extend-merge)
      my-contradictory? (default-contradictory?)]
  (-> (make-system my-merge my-contradictory?)
      (add-value :cell #{:foo})
      (add-value :cell #{:bar})))
;; => throws ExceptionInfo Inconsistency



;; GRAND PLAN:
;;
;; Make an example with sets and temperatures, only having to define
;; c->f, but in terms of relations, and demonstrating how everything
;; happens as if by magic. In that plan, the first article introduces
;; the concepts of propagation and conflicts, propagators (bijective and
;; injective), and conflicts and datatypes.
