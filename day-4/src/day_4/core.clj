(ns day-4.core
  (:require
    [clojure.core :as core]
    [clojure.string :as string]
    [clojure.set :as set]
    ))

(comment

  (def input (slurp "resources/input.txt.dev"))
  (def str-vect (string/split input #"\n"))
  (def first-pair (string/split (first str-vect) #","))

  (def elf-pair-strings (map (fn [elf-pair-string]
                               (string/split elf-pair-string #","))
                             str-vect))
  (def elf-pair-vectors-str (map (fn [elf-pair-string]
                                   (vector
                                     (string/split (first elf-pair-string) #"-")
                                     (string/split (nth elf-pair-string 1) #"-"))
                                   )
                                 elf-pair-strings))
  (def elf-pair-vectors-integers (map (fn [elf-pair]
                                        (vector
                                          (vector
                                            (Integer/parseInt (first (first elf-pair)))
                                            (Integer/parseInt (nth (first elf-pair) 1)))
                                          (vector
                                            (Integer/parseInt (first (nth elf-pair 1)))
                                            (Integer/parseInt (nth (nth elf-pair 1) 1)))
                                          ))
                                      elf-pair-vectors-str))
  (def first-elf (string/split (first first-pair) #"-"))
  (def second-elf (string/split (nth first-pair 1) #"-"))
  (def elf-1-set (set (range (Integer/parseInt (first first-elf))
                             (+ 1 (Integer/parseInt (nth first-elf 1))))))
  (def elf-2-set (set (range (Integer/parseInt (first second-elf))
                             (+ 1 (Integer/parseInt (nth second-elf 1))))))
  (def subset-1-of-2? (set/subset? elf-1-set elf-2-set))
  (def subset-2-of-1? (set/subset? elf-2-set elf-1-set))
  (def overlapping? (or subset-1-of-2? subset-2-of-1?))
  )

;;Alternativ lösning på del 1 utan att använda mängder
(defn between? [x start stop]
  (assert (<= start stop))
  (and (>= x start) (<= x stop))
  )

(defn count-true-elements [the-list]
  (reduce (fn [val m] (if (= true m)
                        (inc val)
                        val))
          0
          the-list))
;(count-true-elements '(true false true))
;(reduce + 0 '(1 2 3 4 5))                                   ;;jmf






;;----------
;om första.start >= andra.start & första.start <= andra.slut & första.slut >= andra.start & första.slut
;om (between? första.start andra.start andra.stop)
;och (between? första.slut andra.start andra.stop)
;eller
; om (between? andra.start första.start första.stop)
;och (between? andra.slut första.start första.stop)
;

(def elf-pair-strings (map (fn [elf-pair-string]
                             (string/split elf-pair-string #",")) str-vect))

(defn -main
  "Solution for day 4 in Advent of code 2022"
  [& args]
  (let [indata (slurp "resources/input.txt")
        str-vect (string/split indata #"\n")
        elf-pair-strings (map (fn [elf-pair-string]
                                (string/split elf-pair-string #","))
                              str-vect)
        elf-pair-vectors-str (map (fn [elf-pair-string]
                                    (vector
                                      (string/split (first elf-pair-string) #"-")
                                      (string/split (nth elf-pair-string 1) #"-"))
                                    )
                                  elf-pair-strings)
        elf-pair-vectors-integers (map (fn [elf-pair]
                                         (vector
                                           (vector
                                             (Integer/parseInt (first (first elf-pair)))
                                             (Integer/parseInt (nth (first elf-pair) 1)))
                                           (vector
                                             (Integer/parseInt (first (nth elf-pair 1)))
                                             (Integer/parseInt (nth (nth elf-pair 1) 1)))
                                           ))
                                       elf-pair-vectors-str)
        ]
    (println "Part 1: "
             (count-true-elements
               (map (fn [elf-pair]
                      (let [[elf-1 elf-2] elf-pair
                            [e1-start e1-stop] elf-1
                            [e2-start e2-stop] elf-2]
                        (or
                          (and (between? e1-start e2-start e2-stop)
                               (between? e1-stop e2-start e2-stop))
                          (and (between? e2-start e1-start e1-stop)
                               (between? e2-stop e1-start e1-stop)
                               ))))
                    elf-pair-vectors-integers)
               )
             )                                              ;;599

    (println "Part 2: "
             (count-true-elements
               (map (fn [elf-pair]
                      (let [[elf-1 elf-2] elf-pair
                            [e1-start e1-stop] elf-1
                            [e2-start e2-stop] elf-2]
                        (or
                          (or (between? e1-start e2-start e2-stop)
                              (between? e1-stop e2-start e2-stop))
                          (or (between? e2-start e1-start e1-stop)
                              (between? e2-stop e1-start e1-stop)
                              ))))
                    elf-pair-vectors-integers)
               ))                                           ;;928
    ))

comment
((-main)
 )