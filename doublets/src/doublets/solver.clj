(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn one-letter-difference? [word other]
  (loop [number-of-differences 0
         letters-left (seq word)
         other-letters-left (seq other)]
    (let [[l & r] letters-left
          [ol & or] other-letters-left
          next-number (+ number-of-differences 1)]
      (cond
        (not= (count letters-left) (count other-letters-left)) false
        (> number-of-differences 1) false
        (and (= number-of-differences 0) (empty? letters-left)) false
        (and (= number-of-differences 1) (empty? letters-left)) true
        :else (if (= l ol) (recur number-of-differences r or) (recur next-number r or))))))

(defn find-closest-words [word]
  (let [one-letter-difference'? (fn [w] (one-letter-difference? word w))]
    (filter one-letter-difference'? words)))

(declare get-path)

(defn find-candidates-doublets [word2 path candidates parsed-words]
  (->> candidates
       (map #(get-path word2 (conj path %) parsed-words))
       (filter #(not (empty? %)))))

(defn get-path
  ([word1 word2] (get-path word2 (seq [word1]) #{word1 word2}))
  ([word2 path parsed-words]
   (let [[head & _] path
         candidates (find-closest-words head)
         branches (find-candidates-doublets word2 path candidates
                                            (into #{} (concat parsed-words candidates)))]
     (cond
       (some #(= % word2) candidates) (reverse (conj path word2))
       (every? #(contains? parsed-words %) candidates) '()
       :else (first (reverse (sort #(compare (count %1) (count %2)) branches)))))))

(defn doublets [word1 word2]
  (vec (get-path word1 word2)))