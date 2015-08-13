(ns numbers.core
  (require [clojure.string :as string]))

(declare int->str)

(def sizes
  "Table of numeric units and sizes."
  [[:trillion 1000000000000]
   [:billion 1000000000]
   [:million 1000000]
   [:thousand 1000]
   [:hundred 100]
   [:ten 10]
   [:one 1]])

(def names
  "Table of numeric units and names"
  {:teen ["eleven" "twelve" "thirteen" "fourteen" "fifteen"
          "sixteen" "seventeen" "eighteen" "nineteen"]
   :ten ["ten" "twenty" "thirty" "forty" "fifty"
         "sixty" "seventy" "eighty" "ninety"]
   :one ["one" "two" "three" "four" "five"
         "six" "seven" "eight" "nine"]})

(defn divide
  "Performs: x/y = [z,r] where r is the remainder."
  [x y]
  (let [remainder (mod x y)]
    [(/ (- x remainder) y) remainder]))

(defn teenify
  "Substitutes [:ten 1] [:one n] in components for [:teen n]."
  [components]
  (let [[[one x] [ten y] & more] (reverse components)]
    (if (and (= :ten ten) (= 1 y))
      (reverse (into more [[:teen x]]))
      components)))

(defn andify
  "Substitutes [:hundred n] ... for [:hundred n] [:str \"and\"] ..."
  [components]
  (reduce (fn [acc [unit n :as pair]]
            (into acc
                  (or (if (and (not (empty? acc))
                               (= :hundred unit))
                        [[:str "and"] pair])
                      [pair])))
          (list)
          (reverse components)))

(defn solve-components
  "Convert n into a table of its component units."
  [n]
  (letfn [(accumulate-units [[n units] [tag size]]
            (let [[quantity remainder] (divide n size)]
              [remainder (conj units [tag quantity])]))]
    (->> (reduce accumulate-units [n (vector)] sizes)
         (last)
         (filter (fn [[tag n]] (> n 0)))
         (teenify)
         (andify))))

(defn str-of-pair
  "String value of the individual component pair."
  [[component v]]
  (if-let [lookup (names component)]
    (lookup (dec v))
    (case component
      :str v
      (str (int->str v) " " (name component)))))

(defn int->str
  "Convert integer n into the English string."
  [n]
  (string/join " " (map str-of-pair (solve-components n))))
