(ns day-2.core
  (:require
    [clojure.core :as core]
    [clojure.core.reducers :as r]
    [clojure.string :as string]
    ))

(def a-str "A X\nB Y\nC Z\n")
(def str-vect (string/split a-str #"\n"))
(prn str-vect)
(def list-of-string-vectors (map (fn [s] (string/split s #" ")) str-vect))
(prn list-of-string-vectors)

(defn apply-rules
  "A,X Rock
  B,Y Paper
  C,Z Scissors"
  [p1 p2]
  (case p1
    "A" (case p2
          "X" (+ 1 3)
          "Y" (+ 2 6)
          "Z" (+ 3 0)
          )
    "B" (case p2
          "X" (+ 1 0)
          "Y" (+ 2 3)
          "Z" (+ 3 6)
          )
    "C" (case p2
          "X" (+ 1 6)
          "Y" (+ 2 0)
          "Z" (+ 3 3)
          )
    )
  )
(doseq [v list-of-string-vectors]
  (print (first v) ":" (nth v 1))
  (println " -> "(apply-rules (first v) (nth v 1))))

(map (fn [x] (apply-rules (first x)(nth x 1))) list-of-string-vectors)

(def score (reduce + (map (fn [x] (apply-rules (first x)(nth x 1))) list-of-string-vectors)))
(println score)

(defn -main
  "Solution for day 1 in Advent of code 2022"
  [& args]
  (let [indata (slurp "resources/input.txt")
        str-vect (string/split indata #"\n")
        list-of-string-vectors (map (fn [s] (string/split s #" ")) str-vect)
        ]
    (reduce + (map (fn [x] (apply-rules (first x)(nth x 1))) list-of-string-vectors))
    ))

(-main)