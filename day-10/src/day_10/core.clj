(ns day-10.core
  (:require
    [clojure.core]
    [ysera.test :refer [deftest is is= is-not]]
    [clojure.string :as string]
    ;[clojure.data.json :as json]
    ;    [clojure.set :as set]
    ;    [clojure.spec.alpha :as s]
    ))

(def input (slurp "resources/test.txt"))
(def str-v (string/split-lines input))
(count str-v)
(def input-map-list (map (fn [r]
                           (let [[cmd value] (string/split r #" ")]
                             (if value
                               {:cmd cmd, :value (read-string value)}
                               {:cmd cmd}))) str-v))
(reverse (conj '(3 2 1) 4))
(empty? nil)
;Add to input map how many cycles each command will use before the result of it is available
(def complete-data-structure (loop [input input-map-list
                                    output-map-list ()
                                    cycles 0
                                    x-reg 1]
                               (let [new-cycles (+ cycles (if (= ((first input) :cmd) "addx") 3 1))
                                     new-map (assoc (first input) :last-cycle-used new-cycles)
                                     last-map (first output-map-list)
                                     new-map (assoc new-map :x-reg (cond (empty? last-map) x-reg
                                                                         (= "addx" (last-map :cmd)) (+ x-reg (last-map :value))
                                                                         (= "noop" (last-map :cmd)) x-reg))]
                                 (if (= 1 (count input))
                                   (into [] (reverse output-map-list))
                                   (recur (rest input)
                                          (conj output-map-list new-map)
                                          new-cycles
                                          (new-map :x-reg))))))




(contains? #{20 40 60} 20)




(defn -main
  "Solution for day 10"
  [& args]
  (println "Hello, World!"))
