(ns p-p-p-pokerface)

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)


(defn rank [card]
  (let [[crank _] card         
        big-cards {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit crank) 
      (Integer/valueOf (str crank))
      (get big-cards crank))))

(defn suit [card] 
  (let [[_ csuit] card]
    (str csuit)))

(defn ranks [hand]
  (map rank hand))

(defn sorted-count-of-similar[hand]
  (sort (vals (frequencies (ranks hand)))))

(defn x-of-a-kind? [hand x] 
  (== (apply max (vals (frequencies (ranks hand)))) x))

(defn pair? [hand]
  (x-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (x-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (x-of-a-kind? hand 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= (sorted-count-of-similar hand) '(2 3)))

(defn two-pairs? [hand]
  (or (= (sorted-count-of-similar hand) '(1 2 2))
      (= (sorted-count-of-similar hand) '(1 4))))

(defn basic-straight? [numbers]
   (let [min-number (apply min numbers)]
     (= (sort numbers) (range min-number (+ min-number 5)))))

(defn straight? [hand]
  (let [numbers (ranks hand)] 
    (or (basic-straight? numbers)
        (if (contains? (set numbers) 1) 
          (basic-straight?(replace {1 14} numbers)) 
          (basic-straight? (replace {14 1} numbers)))
        )))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand] true)

(defn value [hand]
  (let [hand-fits? (fn[[x _]] (x hand))
        checkers #{[high-card? 0] [pair? 1] [two-pairs? 2]
                   [three-of-a-kind? 3] [straight? 4]
                   [flush? 5] [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        found-hand (filter hand-fits? checkers)
        hand-value (map second found-hand)]
   (apply max hand-value)))
