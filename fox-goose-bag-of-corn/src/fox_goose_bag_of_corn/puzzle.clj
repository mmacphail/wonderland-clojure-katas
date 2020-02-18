(ns fox-goose-bag-of-corn.puzzle)

(def actions [:embark :disembark])
(def directions [:left :right])
(def things [:fox :goose :corn])

(def possibilities
  (->> (into [] (for [action actions
                      direction directions
                      thing things]
                  [action direction thing]))
       (concat (for [action actions
                     direction directions]
                 [action direction]))
       (mapv (fn [[action direction thing]]
               (if (or (= action :disembark) (nil? thing))
                 [action direction]
                 [action direction thing])))
       set
       vec
       sort
       reverse))

(defn is-done? [history]
  (let [[_ _ right-bank] (last history)]
    (= #{:fox :goose :corn :you} (set right-bank))))

(def all-keys (into [] (concat [:fox :goose :corn] [:you :boat])))

(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

(defn depart-from-bank
  ([bank thing]
   (filterv #(and (not= :you %) (not= thing %)) bank))
  ([bank]
   (filterv #(not= :you %) bank)))

(defn depart-from-bank-fn
  ([thing]
   (fn [_ bank] (depart-from-bank bank thing)))
  ([]
   (fn [_ bank] (depart-from-bank bank))))

(defn arrive-in-bank [[_ _ thing] bank]
  (if (nil? thing)
    (into [] (concat bank [:you]))
    (into [] (concat [thing] bank [:you]))))

(def empty-boat [:boat])

(defn go-in-boat
  ([thing] [:boat :you thing])
  ([] [:boat :you]))

(defn do-not-change-bank [_ bank] bank)

(defn update-situation [history left-bank-fn updated-boat right-bank-fn]
  (let [current-situation (last history)
        [left-bank boat right-bank] current-situation]
    (conj history
          [(left-bank-fn boat left-bank)
           updated-boat
           (right-bank-fn boat right-bank)])))

(defn update-bank-situation [history direction updated-bank-fn updated-boat]
  (let [left-bank-fn (if (= direction :left) updated-bank-fn do-not-change-bank)
        right-bank-fn (if (= direction :right) updated-bank-fn do-not-change-bank)
        boat updated-boat]
    (update-situation history left-bank-fn boat right-bank-fn)))

(defn embark
  ([history direction thing]
   (update-bank-situation history direction (depart-from-bank-fn thing) (go-in-boat thing)))
  ([history direction]
   (update-bank-situation history direction (depart-from-bank-fn) (go-in-boat))))

(defn disembark [history direction]
  (update-bank-situation history direction arrive-in-bank empty-boat))

(defn execute
  ([history action direction]
   (execute history action direction nil))
  ([history action direction thing]
   (case action
     :embark (if (nil? thing)
               (embark history direction)
               (embark history direction thing))
     :disembark (disembark history direction)
     :else (throw (IllegalArgumentException. (str "Invalid action " action))))))

(defn action-is-already-known? [history action-result]
  (some #(= action-result %) history))

(defn fox-eats-goose? [bank]
  (= #{:fox :goose} (set bank)))

(defn goose-eats-corn? [bank]
  (= #{:goose :corn} (set bank)))

(defn something-bad-is-eaten-in-bank? [bank]
  (or (fox-eats-goose? bank) (goose-eats-corn? bank)))

(defn something-bad-is-eaten? [[left-bank _ right-bank]]
  (or (something-bad-is-eaten-in-bank? left-bank)
      (something-bad-is-eaten-in-bank? right-bank)))

(defn illegal-boat-movement? [[left-bank boat right-bank] action]
  (or
    (and (= action :embark) (contains? (set boat) :you))
    (and (= action :disembark) (or (contains? (set left-bank) :you)
                                   (contains? (set right-bank) :you)))))

(defn things-missing-or-duplicated? [situation]
  (let [count-thing (fn [thing] (count (filter #(= % thing) (flatten situation))))
        counted-things (map count-thing all-keys)]
    (some #(not= 1 %) counted-things)))

(defn move-allowed?
  ([history action direction]
   (move-allowed? history action direction nil))
  ([history action direction thing]
   (let [situation (last history)
         action-result (last (execute history action direction thing))]
     (not (or
            (action-is-already-known? history action-result)
            (something-bad-is-eaten? action-result)
            (things-missing-or-duplicated? action-result)
            (illegal-boat-movement? situation action))))))

(defn find-allowed-moves [history]
  (filter #(apply move-allowed? history %) possibilities))

(defn find-plan [history]
  (let [allowed-moves (find-allowed-moves history)
        choices (map #(apply execute history %) allowed-moves)
        plans (map #(find-plan %) choices)]
    (cond
      (is-done? history) history
      (empty? allowed-moves) []
      :else (let [valid-choices (filter #(not (empty? %)) plans)
                  compare-by-plan-size (fn [p1 p2] (compare (count p1) (count p2)))]
              (first (sort compare-by-plan-size valid-choices))))))

(defn river-crossing-plan []
  (find-plan start-pos))
