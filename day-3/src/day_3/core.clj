(ns day-3.core
  (:require
    [clojure.core :as core]
    [clojure.string :as string]
    ))

(comment
  ;; ToDo Make sure the code really splits in half
  ;; ToDo Iterate over testdata and assert that intersection contains only one element

  (def input (slurp "resources/input.txt.dev"))
  (def str-vect (string/split input #"\n"))
  (prn (type (first (first str-vect))))
  (prn (type \H))

  (def bp-1 (first str-vect))
  (def char-vect (-> bp-1
                     count
                     (quot 2)
                     (split-at bp-1)
                     ))
  (def set-1 (set (first char-vect)))
  (def set-2 (set (nth char-vect 1)))
  (def intersect (clojure.set/intersection set-1 set-2))
  (int (first intersect))
  (assert (= 1 (count intersect)))

  )


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
  "Solution for day 2 in Advent of code 2022"
  [& args]
  (let [indata (slurp "resources/input.txt")
        str-vect (string/split indata #"\n")
        ]
    (time
      (do
        (println "Part 1: ")
        (println "Part 2: ")
        ))))

(comment
  (-main)
  )