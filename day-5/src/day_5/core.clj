(ns day-5.core
  (:require
    [clojure.core :as core]
    [clojure.string :as string]
    [clojure.set :as set]
    [clojure.spec.alpha :as s]
    ))

(def v1 (vector \G \T \R \W))
(def v2 (vector \G \C \H \P \M \S \V \W))
(def v3 (vector \C \L \T \S \G \M))
(def v4 (vector \J \H \D \M \W \R \F))
(def v5 (vector \P \Q \L \H \S \W \F \J))
(def v6 (vector \P \J \D \N \F \M \S))
(def v7 (vector \Z \B \D \F \G \C \S \J))
(def v8 (vector \R \T \B))
(def v9 (vector \H \N \W \L \C))

(def state (atom {}))
(defn init-state! []
  (reset! state {:1 v1, :2 v2, :3 v3, :4 v4, :5 v5, :6 v6, :7 v7, :8 v8, :9 v9}))

(comment
  (def input (slurp "./resources/input.txt.dev"))
  (def rows (string/split input #"\n"))
  (count rows)
  (map (fn [m]
         (eval (read-string (string/replace
                              m #"(move) (\d+) from (\d) to (\d)"
                              "($1! $2 $3 $4)")))
         )
       rows)
  (eval (read-string "(move! 1 2 3)"))
  (eval (read-string (string/replace "move 10 from 4 to 3" #"(move) (\d+) from (\d) to (\d)" "($1! $2 $3 $4)")))
  )

(defn move! [nbr from-stack-id to-stack-id]
  (let [from-stack-kw (keyword (str from-stack-id))
        to-stack-kw (keyword (str to-stack-id))]
    (assert (contains? @state from-stack-kw))
    (assert (contains? @state to-stack-kw))
    (assert (<= nbr (count (@state from-stack-kw))))
    (loop [x nbr]
      (let [from-stack (@state from-stack-kw)
            to-stack (@state to-stack-kw)
            crate (last from-stack)]
        (swap! state assoc from-stack-kw (pop from-stack))
        (swap! state assoc to-stack-kw (conj to-stack crate))
        (when (< 1 x)
          (recur (dec x))))
      )))

(defn move-multi! [nbr from-stack-id to-stack-id]
  (let [from-kw (keyword (str from-stack-id))
        from-stack (@state from-kw)
        to-kw (keyword (str to-stack-id))
        to-stack (@state to-kw)
        crates (subvec from-stack (- (count from-stack) nbr) (count from-stack))
        new-to-stack (into to-stack crates)
        new-from-stack (subvec from-stack 0 (-(count from-stack) nbr))]
    (swap! state assoc from-kw new-from-stack)
    (swap! state assoc to-kw new-to-stack)
    ))

(defn part-1 [rows]
  (doseq [m rows]
    (eval (read-string (string/replace
                         m #"(move) (\d+) from (\d) to (\d)"
                         "($1! $2 $3 $4)")))))

(defn part-2 [rows]
  (doseq [m rows]
    (eval (read-string (string/replace
                         m #"(move) (\d+) from (\d) to (\d)"
                         "($1-multi! $2 $3 $4)")))))

(defn top-crates []
  (loop [x 1 result []]
    (if (> x 9)
      result
      (recur (inc x) (conj result (last (@state (keyword (str x))))))))
  )

(defn -main
  "Solution for day 5"
  [& args]
  (let [input (slurp "./resources/input.txt")
        rows (string/split input #"\n")]
    (init-state!)
    (part-1 rows)
    (println "Part 1: " (top-crates))                       ;J C M H L V G M G
    (init-state!)
    (part-2 rows)
    (println "Part 2: " (top-crates))                       ;L V M R W S S P Z
    ))

(comment
  (-main)
  )