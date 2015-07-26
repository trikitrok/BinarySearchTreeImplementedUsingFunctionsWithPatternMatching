(ns bst-with-functions-with-pattern-matching.core
  (:use [defun :only [defun]]))

(def ^:private value :value)
(def ^:private left :left)
(def ^:private right :right)

(defn singleton [val] {:value val})

(defun in-order
       ([tree :guard nil?] [])

       ([tree] (concat (in-order (left tree))
                       [(value tree)]
                       (in-order (right tree)))))

(defun insert
       ([val tree :guard nil?] (singleton val))

       ([val tree]
         (if (<= val (value tree))
           (assoc tree :left (insert val (left tree)))
           (assoc tree :right (insert val (right tree))))))

(defun from-list
       ([elems :guard empty?] nil)

       ([elems]
         (reduce #(insert %2 %1) (singleton (first elems)) (rest elems))))