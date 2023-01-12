(ns day-2.core
  (:require
    [clojure.core :as core]
    [clojure.string :as string]
    ))

(comment
  (def a-str "A X\nB Y\nC Z\n")
  (def str-vect (string/split a-str #"\n"))
  (prn str-vect)
  (def list-of-string-vectors (map (fn [s] (string/split s #" ")) str-vect))
  (prn list-of-string-vectors)

  Scoring rules as maps
  (def score-map {"A" {"X" (+ 1 3)
                       "Y" (+ 2 6)
                       "Z" (+ 3 0)}
                  "B" {"X" (+ 1 0)
                       "Y" (+ 2 3)
                       "Z" (+ 3 6)}
                  "C" {"X" (+ 1 6)
                       "Y" (+ 2 0)
                       "Z" (+ 3 3)}
                  })

  ((score-map "A") "X")
  (-> "A"
      score-map
      (get "X"))

  )
(defn apply-scoring-rules
  "A,X Rock
  B,Y Paper
  C,Z Scissors"
  [p1 p2]
  (let [score-map {"A" {"X" (+ 1 3)
                        "Y" (+ 2 6)
                        "Z" (+ 3 0)}
                   "B" {"X" (+ 1 0)
                        "Y" (+ 2 3)
                        "Z" (+ 3 6)}
                   "C" {"X" (+ 1 6)
                        "Y" (+ 2 0)
                        "Z" (+ 3 3)}
                   }]
    ((score-map p1) p2)
    ))
(defn apply-scoring-rules-case-style
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
    ))
(defn do-translate [p1 strategy]
  "Input semantics:
  p1: A Rock, B Paper, C Scissors
  strategy
    X: need to lose
    Y: need to draw
    Z: need to win

  Output semantics
  X: Rock, Y: Paper, Z: Scissors"

  [p1 strategy]
  (case p1
    "A" (case strategy
          "X" "Z"
          "Y" "X"
          "Z" "Y"
          )
    "B" strategy
    "C" (case strategy
          "X" "Y"
          "Y" "Z"
          "Z" "X"
          )
    )
  )

(comment Test do-translate
         (do
           (assert (= (do-translate "A" "X") "Z"))
           (assert (= (do-translate "B" "Y") "Y"))
           (assert (= (do-translate "C" "Z") "X"))
           (assert (= (do-translate "B" "X") "X"))
           (assert (= (do-translate "A" "Z") "Y"))
           )
         )

(defn translate-strategy-to-move [list-of-string-vectors]
  (map (fn [m]
         (vector (first m) (do-translate (first m) (nth m 1)))
         )
       list-of-string-vectors)
  )
(comment
  (vector "A" "B")
  (doseq [v list-of-string-vectors]
    (println (first v) ":" (nth v 1)
             " -> " (apply-scoring-rules (first v) (nth v 1))))

  (map (fn [x] (apply-scoring-rules (first x) (nth x 1))) list-of-string-vectors)
  (def part2 (translate-strategy-to-move list-of-string-vectors))
  (prn part2)

  (-> "resources/indata.txt.dev"
      slurp
      (string/split #"\n")
      println)
  (->> "asdf"
       string/upper-case
       println)
  (def score (reduce + (map (fn [x] (apply-scoring-rules (first x) (nth x 1))) list-of-string-vectors)))
  (println score)
  )

(defn -main
  "Solution for day 2 in Advent of code 2022"
  [& args]
  (let [indata (slurp "resources/input.txt")
        str-vect (string/split indata #"\n")
        list-of-string-vectors (map (fn [s] (string/split s #" ")) str-vect)
        ]
    (time
      (do
        (println "Part 1: "
                 (reduce + (map (fn [string-vector] (apply-scoring-rules (first string-vector) (nth string-vector 1))) list-of-string-vectors))
                 )
        (println "Part 2: "
                 (reduce + (map (fn [x] (apply-scoring-rules (first x) (nth x 1))) (translate-strategy-to-move list-of-string-vectors)))
                 )))))

(comment
  (-main)
  )