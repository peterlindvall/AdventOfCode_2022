(ns day-3.core
  (:require
    [clojure.core :as core]
    [clojure.string :as string]
    [clojure.set :as set]
    ))

(comment

  (def input (slurp "resources/input.txt.dev"))
  (def str-vect (string/split input #"\n"))
  (prn (type (first (first str-vect))))
  (prn (-> str-vect
           (first)
           (first)
           (type)))
  (prn (type \H))

  (def bp-1 (first str-vect))
  (def char-vect (-> bp-1
                     count
                     (quot 2)
                     (split-at bp-1)
                     ))
  ;; A list of vectors with two lists of characters (the two halves)
  (def half-string-vectors (map (fn [m] (-> m
                                            count
                                            (quot 2)
                                            (split-at m)
                                            ))
                                str-vect))

  ;; A list of vectors with two sets representing the two halves
  (def intersection (map (fn [m]
                           ;(vector (set (first m)) (set (nth m 1))))
                           (set/intersection (set (first m)) (set (nth m 1))))
                         half-string-vectors))
  (prn intersection)
  (reduce + (map (fn [m] (get-score (first m))) half-string-sets))
  (assert (= (count (first char-vect)) (count (nth char-vect 1))))
  (def set-1 (set (first char-vect)))
  (def set-2 (set (nth char-vect 1)))
  (def intersect (set/intersection set-1 set-2))
  (int (first intersect))
  (assert (= 1 (count intersect)))

  (println (take 3 str-vect))
  (println (take 3 (pop str-vect)))
  (rest str-vect)

  (defn group-in-three-v1 [coll]
    ;;ToDo Varför måste jag skapa vectorer av vectorer?
    (let [s (seq coll)
          cnt (count s)]
      (if (> cnt 3)
        (cons (vector (first s) (nth s 1) (nth s 2))
              (group-in-three (rest (rest (rest s)))))
        (if (= 3 cnt)
          (vector (vector (first s) (nth s 1) (nth s 2)))
          (if (= 2 cnt)
            (vector (vector (first s) (nth s 1)))
            (vector (vector (first s))))))))


  (group-in-three str-vect)
  (group-in-three (pop str-vect))
  (group-in-three (pop (pop str-vect)))

  (def string-triplets (group-in-three str-vect))
  (def badges (map (fn [m]
                     (let [[a b c] m]
                       (set/intersection (set a) (set b) (set c)))) string-triplets))

  (reduce + (map (fn [m]
                   (if (< 0 (count m))
                     (get-score (first m))
                     0))
                 badges))
  )                                                         ;;End of comment

(defn group-in-three [coll]
  (loop [xs coll
         result ()]
    (let [[a b c] xs]
      (if (> 3 (count xs))
        (conj result (vector a b c))
        (recur (rest (rest (rest xs))) (conj result (vector a b c))))
      )))

(defn get-score [chr]
  (if (> (int chr) (int \Z))
    (- (int chr) 96)
    (- (int chr) 38)
    ))

(comment
  ;;Test get-score
  (do
    (assert (= 1 (get-score \a)))
    (assert (= 26 (get-score \z)))
    (assert (= 27 (get-score \A)))
    (assert (= 52 (get-score \Z)))
    )
  )


(defn -main
  "Solution for day 3 in Advent of code 2022"
  [& args]
  (let [indata (slurp "resources/input.txt")
        str-vect (string/split indata #"\n")
        half-string-vectors (map (fn [m] (-> m
                                             count
                                             (quot 2)
                                             (split-at m)
                                             )) str-vect)
        misplaced-items (map (fn [m]
                             (set/intersection (set (first m)) (set (nth m 1))))
                           half-string-vectors)
        groups-of-three (group-in-three str-vect)
        badges (map (fn [m]
                      (let [[a b c] m]
                        (set/intersection (set a) (set b) (set c)))) groups-of-three)
        ]
    ;;Assert string lengths
    (map (fn [m]
           (assert (= (count (first m)) (count (nth m 1)))))
         half-string-vectors)

    ;;Assert intersection cardinality for string halves sets
    (map (fn [m]
           (assert (= 1 (count m))))
         misplaced-items)

    ;;Asset that all groups of three have one badge
    (map (fn [m]
           (assert (= 1 (count m))))
         badges)

    (println "Part 1: "
             (reduce + (map (fn [m] (get-score (first m))) misplaced-items))
             )

    (println "Part 2: "
             (reduce + (map (fn [m]
                              (if (< 0 (count m))
                                (get-score (first m))
                                0))
                            badges)))
    ))

;; Part 1:  7428
;; Part 2:  2650

(comment
  (-main)
  )