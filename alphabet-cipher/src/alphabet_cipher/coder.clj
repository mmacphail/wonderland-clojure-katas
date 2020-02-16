(ns alphabet-cipher.coder)

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(def alphabet (char-range \a \z))

(def a-index (int \a))

(defn alphabet-index [char]
  (- (int char) a-index))

(defn rotate [row col]
 (let [row-index (alphabet-index row)
       col-index (alphabet-index col)]
   (nth (cycle alphabet) (+ col-index row-index))))

(defn encode [keyword message]
  (let [keywords (cycle keyword)
        chars (map #(rotate %1 %2) message keywords)]
    (apply str chars)))

(defn decode [keyword message]
  "decodeme")

(defn decipher [cipher message]
  "decypherme")

