(ns alphabet-cipher.coder)

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(def alphabet (char-range \a \z))

(def a-index (int \a))

(defn char-index [char]
  (- (int char) a-index))

(defn alphabet-from [char]
  (->> (cycle alphabet)
       (drop (char-index char))
       (take (count alphabet))))

(defn rotate [row col]
  (let [row-index (char-index row)
        col-index (char-index col)]
    (nth (cycle alphabet) (+ col-index row-index))))

(defn counter-rotate [k m]
  (let [m-index (.indexOf (alphabet-from k) m)]
    (nth alphabet m-index)))

(defn encode [keyword message]
  (let [keywords (cycle keyword)
        chars (map rotate message keywords)]
    (apply str chars)))

(defn decode [keyword message]
  (let [keywords (cycle keyword)
        chars (map counter-rotate keywords message)]
    (apply str chars)))

(defn decipher [cipher message]
  "decypherme")

