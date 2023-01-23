(ns day-7.core
  (:require
    [clojure.core]
    [clojure.string :as string]
    [clojure.data.json :as json]
    ;    [clojure.set :as set]
    ;    [clojure.spec.alpha :as s]
    ))

(def pwd (atom []))
(def fs (atom {}))
(defn init-state! []
  (reset! pwd ["root"])
  (reset! fs {:root 0
              })
  )

(comment
  (init-state!)
  (deref fs)
  (sort > (vals @fs))
  (create-directory ["root" "bcfwbq" "gpbswq" "jnh" "fzplg"])
  (deref pwd)
  (if (get @fs :root)
    (println "Found")
    (println "Not found"))
  (swap! fs assoc :root-aa-a 111)
  (swap! fs update-in [:root-aa] + 111)
  (swap! fs update-in [:root] + 111)
  (swap! fs assoc :root-aa-b 0)
  (swap! fs update-in [:root-aa-b] + 222)
  (swap! fs update-in [:root-aa] + 222)
  (swap! fs update-in [:root] + 222)

  (def fs-copy @fs)
  (-> fs-copy :sub-dirs (get 0) :dir-name)
  (-> fs-copy :sub-dirs (count))
  (swap! fs assoc-in [:sub-dirs 1] {:dir-name "b"
                                    :size     333           ;;of files in this directory and of subdirectories
                                    :files    [{:name "testfile-3"
                                                :size 333
                                                }]
                                    :sub-dirs []})

  )
;;ToDo Learn how to use get-in and assoc-in
(comment
  (spit "resources/test.json" (json/write-str @fs))
  )

;;If subdirectories have unique names we do not need to keep track of hierarchy
;;We could keep track of directories by letting the name be the entire path

(defn build-directory-name [path]
  (reduce (fn [a b]
            (string/join "-" [a b])) path)
  )

;(build-directory-name ["/" "bcfwbq" "gpbswq" "jnh" "fzplg"])

(defn add-file!
  "Increases the size of all directories in the path with file size"
  [path file-name size]
  (loop [depth 1]
    (let [key (keyword (build-directory-name (subvec path 0 depth)))]
      (swap! fs update-in [key] + (read-string size))
      (when (> (count path) depth)
        (recur (inc depth))))))

(defn update-fs!
  "Check if all directories in path exists
  If they don't, create them with size 0"
  [path]
  (when (< 1 (count path))
    (loop [depth 2]
      (let [key (keyword (build-directory-name (subvec path 0 depth)))]
        (if-not (get @fs key)
          (swap! fs assoc key 0)
          )
        (when (> (count path) depth)
          (recur (inc depth)))))))


(defn cd! [dir]
  (let [old-pwd @pwd]
    (cond
      (= ".." dir) (do
                     (assert (< 1 (count old-pwd)))
                     (reset! pwd (pop old-pwd)))
      (= "/" dir) (reset! pwd ["root"])
      :otherwise (do (reset! pwd (conj old-pwd dir))
                     (update-fs! @pwd))
      )))

(comment
  (init-state!)
  (deref pwd)
  (cd! "/")
  (cd! "a")
  (cd! "b")
  (cd! "c")
  (cd! ".."))

(defn sum-all-below [threshold coll]
  (reduce (fn [sum el]
            (if (>= threshold (val el))
              (+ sum (val el))
              sum))
          0
          coll))

(def disk-size 70000000)
(def used-storage-before-delete 41518953)                   ;(@fs :root))
(def free-space-before-delete (- disk-size used-storage-before-delete))
(def free-space-required-to-update 30000000)
(def space-to-free-up (- free-space-required-to-update free-space-before-delete))

(defn -main
  "Solutions for day 7"
  [& args]
  (let [input (slurp "resources/input.txt")
        str-vect (string/split input #"\n")]
    (init-state!)
    (doseq [line str-vect]
      (let [tokens (string/split line #" ")]
        (cond
          (= "cd" (tokens 1)) (cd! (tokens 2))
          (nat-int? (read-string (tokens 0))) (add-file! @pwd (tokens 1) (tokens 0))
          ))))
  (println "Part 1:" ;1581595
           (sum-all-below 100000 @fs))

  (println "Part 2:" ;1544176
           (first (sort (vals (filter #(>= (second %) space-to-free-up) @fs))))
           ))

(comment
  (-main)
  )

(comment
  (reset! fs {:dir-name "/"
              :size     333                                 ;;of files in this directory and of subdirectories
              :files    [{:name "testfile-1"
                          :size 111
                          }]
              :sub-dirs [{:dir-name "a"
                          :size     222                     ;;of files in this directory and of subdirectories
                          :files    [{:name "testfile-2"
                                      :size 222
                                      }]
                          :sub-dirs []}]})
  )
