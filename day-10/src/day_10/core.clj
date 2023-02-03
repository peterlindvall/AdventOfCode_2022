(ns day-10.core
  (:require
    [clojure.core]
    [ysera.test :refer [deftest is is= is-not]]
    [clojure.string :as string]
    ;[clojure.data.json :as json]
    ;    [clojure.set :as set]
    ;    [clojure.spec.alpha :as s]
    ))

(def sprite-pos (atom 3))                                   ;sprite is three pixels wide. Position indicates center
;(def cycle-count 1)                                         ;during every cycle the crt draws the pixel in pos sprite-1
(def cursor-pos (atom 0))                                   ;iterates from 0-39

(defn draw [sprite cursor]
  (if (contains? #{(dec sprite) sprite (inc sprite)} cursor)
    (print "#")
    (print ".")))

(draw @sprite-pos @cursor-pos)
(deref sprite-pos)
(swap! sprite-pos inc)
(reset! sprite-pos 1)

(def input (slurp "resources/input.txt"))
(def str-v (string/split-lines input))
(def input-map-list (map (fn [r]
                           (let [[cmd value] (string/split r #" ")]
                             (if value
                               {:cmd cmd, :value (read-string value)}
                               {:cmd cmd}))) str-v))

(def add-cycle-cycles-and-x-reg (loop [input input-map-list
                                       output-map-list ()
                                       cycles 0
                                       x-reg 1]
                                  (let [current-map (first input)
                                        new-cycles (+ cycles (if (= "addx" (current-map :cmd)) 2 1))
                                        last-map (first output-map-list)
                                        map-last-cycle-used {:last-cycle-used new-cycles}
                                        map-x-reg {:x-reg (cond (empty? last-map) x-reg
                                                                (= "addx" (last-map :cmd)) (+ x-reg (last-map :value))
                                                                (= "noop" (last-map :cmd)) x-reg)}]
                                    (if (= 1 (count input))
                                      (reverse output-map-list)
                                      (recur (rest input)
                                             (conj output-map-list (conj current-map map-x-reg map-last-cycle-used))
                                             new-cycles
                                             (map-x-reg :x-reg))))))

(def poi-seq (take 100 (filter #(or (= 20 %) (= 0 (mod (- % 20) 40))) (range))))

(def add-signal-strength (loop [poi poi-seq
                                maps add-cycle-cycles-and-x-reg
                                new-map-list ()]
                           (let [current-map (first maps)
                                 current-poi (first poi)
                                 at-poi? (<= current-poi (current-map :last-cycle-used))
                                 signal-strength {:signal-strength (* current-poi (current-map :x-reg))}
                                 next-poi (if at-poi? (rest poi) poi)]
                             (if (= 1 (count maps))
                               new-map-list
                               (recur next-poi
                                      (rest maps)
                                      (if at-poi? (conj new-map-list (conj current-map signal-strength))
                                                  (conj new-map-list current-map)))))))


(defn -main
  "Solution for day 10"
  [& args]
  (println "Part 1:"
           (reduce (fn [sum el]
                     (+ sum (el :signal-strength)))
                   0
                   (filter #(contains? % :signal-strength) add-signal-strength))))

(comment
  (-main)
  )