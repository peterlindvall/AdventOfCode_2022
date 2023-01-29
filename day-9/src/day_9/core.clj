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
    (swap! state assoc :h-pos [150 250])
    (swap! state assoc :t-pos [150 250])
    (swap! state assoc :s-pos [150 250])
    (swap! state assoc :visited #{[150 250]})
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

(defn move-head! [h-pos dir]
  (let [[h-row h-col] h-pos]
    (swap! state assoc :h-pos
           (case dir
             "R" [h-row (inc h-col)]
             "L" [h-row (dec h-col)]
             "U" [(dec h-row) h-col]
             "D" [(inc h-row) h-col]
             ))))
(defn move-tail! [head-pos tail-pos]
  "Om på samma rad -> flytta en kolumn närmare
   Om på olika rader men samma kolumn -> flytta en rad närmare
   Om på olika rader och olika kolumner -> flytta en rad och en kol närmare"
  (let [[h-row h-col] head-pos
        [t-row t-col] tail-pos]
    ;(println "move-tail! h:" head-pos "t:" tail-pos)
    (swap! state assoc :t-pos
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
             :else (assert (= "fail in move-tail logic" ""))))))
(defn move-rope! [move]
  (let [[dir steps] move]
    (loop [step 1]
      (move-head! (@state :h-pos) dir)
      ;(println "Dir:" dir "Head:" (@state :h-pos))
      (when (not (adjacent? (@state :h-pos) (@state :t-pos)))
        (move-tail! (@state :h-pos) (@state :t-pos))
        (swap! state update :visited conj (@state :t-pos)))
      (when (< step steps)
        (recur (inc step)))
      )))
(defn -main
  "Solution for day 9"
  [& args]
  (init-state! "resources/input.txt")
  (doseq [move (@state :moves)]
    (move-rope! move))
  (assert (= 5710 (count (@state :visited))))
  (println "Part 1:"
           (count (@state :visited)))
  (println "Part 2:"))

(comment
  (-main)
  )
