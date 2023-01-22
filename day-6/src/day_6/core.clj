(ns day-6.core
  (:require
    [clojure.core :as core]
    ;    [clojure.string :as string]
    ;    [clojure.set :as set]
    ;    [clojure.spec.alpha :as s]
    ))

(comment
  ;bvwbjplbgvbhsrlpgdmjqwftvncz: first marker after character 5
  ;nppdvjthqldpwncqszvftbrmjlhg: first marker after character 6
  ;nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg: first marker after character 10
  ;zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw: first marker after character 11
  )

(defn locate-start-of-packet-marker [str]
  (let [char-vect (into [] str)]
    (loop [index 0]
      (let [[a b c d] (subvec char-vect index (+ 4 index))]
        ;(println a b c d)
        (if (= 4 (count (set [a b c d])))
          (+ 4 index)
          (recur (inc index))))
      )))

(assert (= 5 (locate-start-of-packet-marker "bvwbjplbgvbhsrlpgdmjqwftvncz")))
(assert (= 6 (locate-start-of-packet-marker "nppdvjthqldpwncqszvftbrmjlhg")))
(assert (= 10 (locate-start-of-packet-marker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")))
(assert (= 11 (locate-start-of-packet-marker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")))

(defn locate-start-of-message-marker [str]
  (let [char-vect (into [] str)]
    (loop [index 0]
      (let [[a b c d e f g h i j k l p q] (subvec char-vect index (+ 14 index))]
        ;(println a b c d)
        (if (= 14 (count (set [a b c d e f g h i j k l p q])))
          (+ 14 index)
          (recur (inc index))))
  )))

(assert (= 19 (locate-start-of-message-marker "mjqjpqmgbljsphdztnvjfqwrcgsmlb")))
(assert (= 23 (locate-start-of-message-marker "bvwbjplbgvbhsrlpgdmjqwftvncz")))
(assert (= 23 (locate-start-of-message-marker "nppdvjthqldpwncqszvftbrmjlhg")))
(assert (= 29 (locate-start-of-message-marker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")))
(assert (= 26 (locate-start-of-message-marker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")))



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [input (slurp "./resources/input.txt")]
  (println "Part 1:" ;1598
           (locate-start-of-packet-marker input))
(println "Part 2:" ;
         (locate-start-of-message-marker input))))

(comment
  (-main)
  )