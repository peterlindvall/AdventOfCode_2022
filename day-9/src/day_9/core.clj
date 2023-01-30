(ns day-9.core
  (:require
    [clojure.core]
    [ysera.test :refer [deftest is is= is-not]]
    [clojure.string :as string]
    ;[clojure.data.json :as json]
    ;    [clojure.set :as set]
    ;    [clojure.spec.alpha :as s]
    ))


(def state (atom {}))

(defn init-state! [input-path]
  (let [file (slurp input-path)
        str-v (string/split-lines file)
        moves (map (fn [m]
                     (let [dir (re-find #"^." m)
                           steps (read-string (re-find #"\d+" m))]
                       [dir steps])) str-v)]
    (swap! state assoc :moves moves)
    (swap! state assoc :h-pos [15 11])                      ;only used for part 1
    (swap! state assoc :t-pos [15 11])                      ;only used for part 1
    (swap! state assoc :visited #{[15 11]})
    (swap! state assoc :rope [[15 11] [15 11] [15 11] [15 11] [15 11] [15 11] [15 11] [15 11] [15 11] [15 11]])
    ))

(defn count-true
  {:test (fn []
           (is= 3 (count-true '(true false true false true)))
           )}
  [the-coll]
  (count (filter true? the-coll))
  )

(defn adjacent?
  {:test (fn []
           (is= true (adjacent? [3 3] [3 3]))
           (is= true (adjacent? [3 3] [3 4]))
           (is= true (adjacent? [3 3] [3 2]))
           (is= true (adjacent? [3 3] [2 3]))
           (is= true (adjacent? [3 3] [4 3]))
           (is= true (adjacent? [3 3] [4 4]))
           (is= true (adjacent? [3 3] [2 2]))
           (is= false (adjacent? [3 3] [1 3]))
           (is= false (adjacent? [3 3] [3 1]))
           (is= false (adjacent? [3 3] [4 1]))
           )}
  [head tail]
  (let [[h-row h-col] head
        [t-row t-col] tail]
    (and (>= 1 (abs (- h-col t-col))) (>= 1 (abs (- h-row t-row))))
    ))

(defn move-head [h-pos dir]
  (let [[h-row h-col] h-pos]
    (case dir
      "R" [h-row (inc h-col)]
      "L" [h-row (dec h-col)]
      "U" [(dec h-row) h-col]
      "D" [(inc h-row) h-col]
      )))

(defn move-tail [head-pos tail-pos]
  "Om på samma rad -> flytta en kolumn närmare
   Om på olika rader men samma kolumn -> flytta en rad närmare
   Om på olika rader och olika kolumner -> flytta en rad och en kol närmare"
  (let [[h-row h-col] head-pos
        [t-row t-col] tail-pos]
    ;(println "move-tail! h:" head-pos "t:" tail-pos)
    (cond
      (= h-row t-row) [t-row (if (> h-col t-col) (inc t-col) (dec t-col))]
      (and (not= h-row t-row) (= h-col t-col)) [(if (> h-row t-row)
                                                  (inc t-row)
                                                  (dec t-row)) t-col]
      (and (not= h-row t-row) (not= h-col t-col)) [(if (> h-row t-row)
                                                     (inc t-row)
                                                     (dec t-row))
                                                   (if (> h-col t-col)
                                                     (inc t-col)
                                                     (dec t-col))]
      :else (assert (= "fail in move-tail" "")))))
(defn move-rope! [move]
  (let [[dir steps] move]
    (loop [step 1]
      (swap! state assoc :h-pos
             (move-head (@state :h-pos) dir))
      ;(println "Dir:" dir "Head:" (@state :h-pos))
      (when (not (adjacent? (@state :h-pos) (@state :t-pos)))
        (swap! state assoc :t-pos
               (move-tail (@state :h-pos) (@state :t-pos)))
        (swap! state update :visited conj (@state :t-pos)))
      (when (< step steps)
        (recur (inc step)))
      )))

;(assoc [[1] [2] [3]] 1 [5])
;[1 5 3]
(defn move-long-rope! [move]
  (let [[dir steps] move]
    (loop [step 1]
      (swap! state assoc-in [:rope 0]
             (move-head (get (@state :rope) 0) dir))
      (loop [rope-pos 1]
        (when (not (adjacent? (get (@state :rope) (dec rope-pos)) (get (@state :rope) rope-pos)))
          (swap! state assoc-in [:rope rope-pos]
                 (move-tail (get (@state :rope) (dec rope-pos)) (get (@state :rope) rope-pos))))
        (when (contains? (@state :rope) (inc rope-pos))
          (recur (inc rope-pos))))
      (swap! state update :visited conj (last (@state :rope)))
      (when (< step steps)
        (recur (inc step))))))

(defn head-pos? [pos]
  (= pos (first (@state :rope))))
(defn tail-pos? [pos]
  (= pos (last (@state :rope))))
(defn rope-pos?
  "is any part of the rope, part from the head, in this pos?"
  [pos]
  (some #(= % pos) (@state :rope)))
(defn print-rope
  "Print a grid 21x26 with the rope"
  []
  (loop [row 0]
    (println
      (string/join
        (for [col (range 27)]
          (cond
            (head-pos? [row col]) "H"
            (tail-pos? [row col]) "T"
            (rope-pos? [row col]) "O"
            :else "."
            ))))
    (when (< row 21)
      (recur (inc row)))))
(print-rope)

(defn -main
  "Solution for day 9"
  [& args]
  (init-state! "resources/input.txt")
  (doseq [move (@state :moves)]
    (move-rope! move))
  (assert (= 5710 (count (@state :visited))))
  (println "Part 1:" ;5710
           (count (@state :visited)))

  (init-state! "resources/input.txt")
  (doseq [move (@state :moves)]
    (move-long-rope! move))
  (println "Part 2:" ;2259
           (count (@state :visited))))

(comment
  (-main)
  )
