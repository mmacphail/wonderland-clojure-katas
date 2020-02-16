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

(defn rotate [k m]
  "Given a key char k and a message char m, gives the nth k char in the m-starting alphabet."
  (nth (alphabet-from m) (.indexOf alphabet k)))

(defn counter-rotate [x y]
  "Given x y chars, gives the nth (index of y in x-starting alphabet) char in the alphabet."
    (nth alphabet (.indexOf (alphabet-from x) y)))

(defn find-smallest-cypher [cypher-str]
  "Given a cypher string, returns the smallest cypher string"
  (let [cypher (seq cypher-str)]
    (loop [pattern []
           rest cypher]
      (let [pattern-string (take (count cypher) (cycle pattern))
            pattern-identified? (= pattern-string cypher)
            [char & r] rest
            new-pattern (conj pattern char)]
        (cond
          pattern-identified? (apply str pattern)
          (empty? rest) cypher-str
          :else (recur new-pattern r))))))

(defn encode [keyword message]
  (let [keywords (cycle keyword)
        chars (map rotate message keywords)]
    (apply str chars)))

(defn decode [keyword message]
  (let [keywords (cycle keyword)
        chars (map counter-rotate keywords message)]
    (apply str chars)))

(defn decipher [cipher message]
  (let [chars (map counter-rotate message cipher)]
    (-> (apply str chars)
        find-smallest-cypher)))