(ns day-1.core
  (:require
    [clojure.core :as core]
    [clojure.core.reducers :as r]
    [clojure.string :as string]
    ))

(def l1 (list 10 20 30))                                    ;60
(def l2 (list 20 30 40))                                    ;90
(def l3 (list 30 40 50))                                    ; 120
(def sums (map + l1 l2 l3)) ; Does not work when using (map + coll) as function iterating over a seq. Use reduce
(println sums)
(first (sort > sums))                                       ; <-------
(def a-str "1\n2\n3\n4\n\n5\n6\n7")
(def str-vect (string/split a-str #"\n\n"))
(prn str-vect)
(def list-of-string-vectors (map (fn [s] (string/split s #"\n")) str-vect))
(prn list-of-string-vectors)

(def list-of-long-lists
  (map (fn [x]
         (map read-string x)
         ) list-of-string-vectors))
(prn list-of-long-lists)

(def list-of-sums (map (fn [x]
                         (reduce + x)
                         )
                       list-of-long-lists))

(prn list-of-sums)
(println (first (sort > list-of-sums)))


(defn -main
  "Solution for day 1 in Advent of code 2022"
  [& args]
  (let [indata (slurp "resources/indata.txt")
        str-vect (string/split indata #"\n\n")
        list-of-string-vectors (map (fn [s]
                                      (string/split (string/trim-newline s) #"\n"))
                                    str-vect)
        list-of-long-lists (map (fn [x]
                                  (map read-string x))
                                list-of-string-vectors)
        list-of-sums (map (fn [x]
                            (reduce + x)
                            )
                          list-of-long-lists)
        [a b c] (sort > list-of-sums)
        ]
    (println "Part 1: " a)
    (println "Part 2: " (+ a b c) )
    ))

;(-main)