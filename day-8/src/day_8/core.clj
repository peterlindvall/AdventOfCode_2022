(ns day-8.core
  (:require
    [clojure.core]
    [clojure.string :as string]
    ;[clojure.data.json :as json]
    ;    [clojure.set :as set]
    ;    [clojure.spec.alpha :as s]
    ))

(def state (atom {}))
(defn init-state! []
  (let [input (slurp "resources/input.txt")
        str-v (string/split-lines input)]
    (swap! state assoc :forest (map (fn [m]
                                      (vec (map #(Integer/parseInt %) (string/split m #"")))) str-v))
    (swap! state assoc :last-row (dec (count (@state :forest))))
    (swap! state assoc :last-col (dec (count (first (@state :forest)))))
    ))

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

(defn on-the-perimeter? [row col]
  (or (= 0 row) (= 0 col)
      (= (@state :last-row) row)
      (= (@state :last-col) col)))

(defn visible?
  ([index]
   (let [[row col] index]
     (visible? row col (@state :forest))))
  ([row col tree-map]
   (or (on-the-perimeter? row col)
       (visible-from-the-north? row col tree-map)
       (visible-from-the-south? row col tree-map)
       (visible-from-the-east? row col tree-map)
       (visible-from-the-west? row col tree-map))))

(comment
  ;count-rows and count-row replaced by (filter visible? (for... in -main
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
  )


;;if not at edge, at least one tree is seen. If the height of the tree seen is lower then recur
(defn tree-count-northbound
  "count n:o of trees seen northbound"
  [row col height forest]
  (if (= 0 row)
    0
    (loop [trees-seen 1
           r row]
      (if (or (>= (get (nth forest (dec r)) col) height) (= 0 (dec r))) ;"next tree higher or equal"
        trees-seen                                          ;won't be able to see anymore trees
        (recur (inc trees-seen) (dec r))))))

;;case 1 - alla är lägre
;row 2
;col 1
;loop-1 (:trees-seen 1, :r 2, "kika på rad 1") :recur
;loop-2 (:trees-seen 2, :r 1, "kika på rad 0)"

;;ToDo helper function that returns height for tree at coordinate

(defn tree-count-southbound
  "Count n:o of trees seen southbound.
  Is not to be called for trees in the southernmost row "
  [row col height forest]
  (loop [trees-seen 1
         r row]
    (if (or (>= (get (nth forest (inc r)) col) height) (= (@state :last-row) (inc r))) ;"next tree higher or equal"
      trees-seen                                            ;won't be able to see anymore trees
      (recur (inc trees-seen) (inc r)))))
(defn tree-count-eastbound
  "count n:o of trees seen eastbound.
  Is not to be called for trees in the easternmost column"
  [row col height forest]
  (loop [trees-seen 1
         c col]
    (if (or (>= (get (nth forest row) (inc c)) height) (= (@state :last-col) (inc c))) ;"next tree higher or equal"
      trees-seen                                            ;can't see anymore trees
      (recur (inc trees-seen) (inc c)))))
(defn tree-count-westbound
  "count n:o of trees seen westbound.
  Is not to be called for trees in the westernmost columnt"
  [row col height forest]
  (loop [trees-seen 1
         c col]
    (if (or (>= (get (nth forest row) (dec c)) height) (= 0 (dec c))) ;"next tree higher or equal"
      trees-seen                                            ;can't see anymore trees
      (recur (inc trees-seen) (dec c)))))
(defn tree-count [row col forest]
  (let [height (get (nth forest row) col)]
    (if (on-the-perimeter? row col)
      0
      (reduce * (list (tree-count-northbound row col height forest)
                      (tree-count-southbound row col height forest)
                      (tree-count-eastbound row col height forest)
                      (tree-count-westbound row col height forest))))))



(defn -main
  [& args]
  args
  (init-state!)
  (let [coordinates (for [row (range 0 (count (@state :forest)))
                          col (range 0 (count (first (@state :forest))))]
                      [row col])]
    (println "Part 1:"
             (count (filter visible? coordinates)))
    (println "Part 2:"
             (apply max (map (fn [index]
                               (let [[row col] index]
                                 (tree-count row col (@state :forest))))
                             coordinates)))))

(comment
  (-main)
  )
