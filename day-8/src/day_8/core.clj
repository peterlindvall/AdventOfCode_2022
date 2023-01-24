(ns day-8.core
  (:require
    [clojure.core]
    [clojure.string :as string]
    ;[clojure.data.json :as json]
    ;    [clojure.set :as set]
    ;    [clojure.spec.alpha :as s]
    ))
(comment
  (def input (slurp "resources/test.txt"))
  (def str-v (string/split-lines input))
  (def tree-v (map (fn [m]
                     (into [] (map (fn [n]
                                     (read-string (str n)))
                                   (into [] m)))
                     ) str-v))
  )

;;Strategy
;;Iterate through "all" trees and look around until it is determined visible or not
;;For each tree
;;get list/vector of elements in each direction NESW
;;if an edge (end of list) is reached before a tree with equal or greater height is reached
;;then the tree is deemed visible from the outside

;;Zero-based indexing

;;example (1,3)(Row,Col)
;x x x x x
;x x x o x
;x x x x x
;x x x x x
;x x x x x

(defn visible-from-the-north? [row col tree-map]
  (let [height (get (nth tree-map row) col)]
    (if (= 0 row)
      true                                                  ;All trees in the north-most row are visible
      (loop [r (dec row)]
        (if (<= height (get (nth tree-map r) col))
          false                                             ;;view is obstructed
          (if (= r 0)
            true                                            ;;reached the edge!
            (recur (dec r))))))))

(defn visible-from-the-south? [row col tree-map]
  (let [height (get (nth tree-map row) col)]
    (if (= (dec (count tree-map)) row)
      true                                                  ;All trees in the south-most row are visible
      (loop [r (inc row)]
        (if (<= height (get (nth tree-map r) col))
          false                                             ;;view is obstructed
          (if (= r (dec (count tree-map)))
            true                                            ;;reached the edge!
            (recur (inc r))))))))

(defn visible-from-the-west? [row col tree-map]
  (let [height (get (nth tree-map row) col)]
    (if (= 0 col)
      true                                                  ;All trees in the west-most row are visible
      (loop [c (dec col)]
        (if (<= height (get (nth tree-map row) c))
          false                                             ;;view is obstructed
          (if (= c 0)
            true                                            ;;reached the edge!
            (recur (dec c))))))))

(defn visible-from-the-east? [row col tree-map]
  (let [height (get (nth tree-map row) col)]
    (if (= (dec (count (first tree-map))) col)
      true                                                  ;All trees in the east-most row are visible
      (loop [c (inc col)]
        (if (<= height (get (nth tree-map row) c))
          false                                             ;;view is obstructed
          (if (= c (dec (count (first tree-map))))
            true                                            ;;reached the edge!
            (recur (inc c))))))))

(defn on-the-perimeter? [row col tree-map]
  (or (= 0 row) (= 0 col)
      (= (dec (count tree-map)) row)
      (= (dec (count (first tree-map))) col)))

(defn visible? [row col tree-map]
  (or (on-the-perimeter? row col tree-map)
      (visible-from-the-north? row col tree-map)
      (visible-from-the-south? row col tree-map)
      (visible-from-the-east? row col tree-map)
      (visible-from-the-west? row col tree-map)))

;(map count (filter visible? tree-v))

(defn count-row [row tree-map]
  (loop [col 0
         visible-count 0]
    (if (= col (count (first tree-map)))
      visible-count
      (recur (inc col) (if (visible? row col tree-map)
                         (inc visible-count)
                         visible-count)))))

(defn count-rows [tree-map]
  (loop [row 0
         accumulated-count 0]
    (if (= row (count tree-map))
      accumulated-count
      (recur (inc row) (+ accumulated-count (count-row row tree-map))))))

;(count-rows tree-v)

;;ToDo Create a list of all indices in map e.g. [1 3], filter that list with visible? and count the result

(defn -main
  [& args]
  (let [input (slurp "resources/input.txt")
        str-v (string/split-lines input)
        tree-v (map (fn [m]
                      (into [] (map (fn [n]
                                      (read-string (str n)))
                                    (into [] m)))
                      ) str-v)]
    (println "Part 1:"
             (count-rows tree-v))
    (println "Part 2:"
             )
    ))

(comment
  (-main)
  )
