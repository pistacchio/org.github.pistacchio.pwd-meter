(ns org.github.pistacchio.pwd-meter)

;; utils
(defn uppercase? [c]
  (<= 65 (int c) 90))

(defn lowercase? [c]
  (<= 97 (int c) 122))

(defn numeric? [c]
  (<= 48 (int c) 57))

(defn alpha? [c]
  (or (uppercase? c) (lowercase? c)))

(defn special? [c]
  (not (or (uppercase? c)
           (lowercase? c)
           (numeric? c))))

(defn num-letters [p fn]
  (count (filter fn p)))


(defn consecutive [fn p]
  (->>
    (partition-by fn p)
    (filter (partial some fn))       
    (map #(dec (count %)))                   
    (reduce +)))

(defn consecutive-sequence [p pattern]
  (reduce
    #(if (re-find (re-pattern (apply str %2)) p)
      (inc %1)
      %1)
    0 (partition 3 1 pattern)))

;; bonus
(defn bonus-length [p]
  (* 4 (count p)))

(defn bonus-case [fn p]
  (let [chars (num-letters p fn)]
    (if (and (> chars 0) (< chars (count p)))
      (* 2 (- (count p) chars))
      0)))

(defn bonus-chars [fn b p]
    (* b (num-letters p fn)))

(defn bonus-middle-num-symbols [p]
  (let [middle (drop-last (rest p))
        letters (filter #(or (numeric? %) (special? %)) middle)]
    (* 2 (count letters))))

(defn bonus-requirements [p]
  (letfn [(score? [fn] (if (> (num-letters p fn) 0) 1 0))]     
     (* 2
        (+ (if (> (count p) 7) 1 0)        
           (score? uppercase?)
           (score? lowercase?)
           (score? numeric?)
           (score? special?)))))

;; malus
(defn malus-letters-only [p]
  (if (every? alpha? p) (count p) 0))

(defn malus-numbers-only [p]
  (if (every? numeric? p) (count p) 0))  
    
(defn malus-repeated-chars [p]
  (- (count p) (count (into #{} (.toLowerCase p)))))

(defn malus-consecutive-lower [p]
  (* 2 (consecutive lowercase? p)))

(defn malus-consecutive-upper [p]
  (* 2 (consecutive uppercase? p)))

(defn malus-consecutive-numeric [p]
  (* 2 (consecutive numeric? p)))

(defn malus-consecutive-sequence-letters [p]
  (* 3 (consecutive-sequence p "abcdifghijklmnopqrstuvwxyz")))

(defn malus-consecutive-sequence-numbers [p]
  (* 3 (consecutive-sequence p "01234567890")))

(defn malus-consecutive-sequence-special [p]
  (* 3 (consecutive-sequence p ")!@#$%^&*()")))

;; aggregation
(defn check-score [p]
  (+
    (+
      (bonus-length p)
      (bonus-case uppercase? p)
      (bonus-case lowercase? p)
      (bonus-chars numeric? 4 p)
      (bonus-chars special? 6 p)
      (bonus-middle-num-symbols p)
      (bonus-requirements p))
    (-
      (malus-letters-only p)
      (malus-numbers-only p)
      (malus-repeated-chars p)
      (malus-consecutive-lower p)
      (malus-consecutive-upper p)
      (malus-consecutive-numeric p)
      (malus-consecutive-sequence-letters p)
      (malus-consecutive-sequence-numbers p))))

