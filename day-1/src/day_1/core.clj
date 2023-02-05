(ns day-1.core
  (:require
    [clojure.core :as core]
    [clojure.string :as string]
    ))
(def strings (-> (slurp "resources/indata.txt")
                 (string/split #"\n\n")))
(def numbers (->> strings
                  (reduce (fn [a v]
                            (conj a (->> (string/split v #"\n")
                                         (reduce (fn [a v]
                                                   (conj a (read-string v)))
                                                 []))))
                          [])))
(def sums (sort > (->> numbers
                       (reduce (fn [a v]
                                 (conj a (apply + v)))
                               []))))
(defn -main
  "Solution for day 1 in Advent of code 2022"
  [& args]
  (println "Part 1: " (first sums))
  (println "Part 2: " (apply + (take 3 sums)))
  )

(comment
  (-main)
  )