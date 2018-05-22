(def asym-hobbit-body-parts [{:name "head" :size 3}
                             {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "left-shoulder" :size 3}
                             {:name "left-upper-arm" :size 3}
                             {:name "chest" :size 10}
                             {:name "back" :size 10}
                             {:name "left-forearm" :size 3}
                             {:name "abdomen" :size 6}
                             {:name "left-kidney" :size 1}
                             {:name "left-hand" :size 2}
                             {:name "left-knee" :size 2}
                             {:name "left-thigh" :size 4}
                             {:name "left-lower-leg" :size 3}
                             {:name "left-achilles" :size 1}
                             {:name "left-foot" :size 2}])

(defn matching-part
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)})

(defn symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (loop [remaining-asym-parts asym-body-parts final-body-parts []]
    (if (empty? remaining-asym-parts)
      final-body-parts
      (let [[part & remaining] remaining-asym-parts]
        (recur remaining
               (into final-body-parts
                     (set [part (matching-part part)])))))))

(symmetrize-body-parts asym-hobbit-body-parts)

(defn better-symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (reduce (fn [final-body-parts part]
            (into final-body-parts (set [part (matching-part part)])))
          []
          asym-body-parts))

(defn hit
  [asym-body-parts]
  (let [sym-parts (better-symmetrize-body-parts asym-hobbit-body-parts)
        body-part-size-sum (reduce + (map :size sym-parts))
        target (rand body-part-size-sum)]
    (loop [[part & remaining] sym-parts accumulated-size (:size part)]
      (if (> accumulated-size target)
        part
        (recur remaining (+ accumulated-size (:size (first remaining))))))))

(hit asym-hobbit-body-parts)

; Exercises
; 1. Use the str, vector, list, hash-map, and hash-set functions.
(str 42)
(vector 1 2 3)
(list 1 2 3)
(hash-map :a 1 :b 2 :c 3)
(hash-set 1 1 2 2 3 3)

; 2. Write a function that takes a number and adds 100 to it.
(defn add100
  "Takes a number and adds 100 to it"
  [n]
  (+ n 100))

(add100 1)

; 3. Write a function, dec-maker, that works exactly like the function
; inc-maker except with subtraction.
(defn dec-maker
  "Create a custom decrementor"
  [dec-by]
  #(- % dec-by))

(def dec9 (dec-maker 9))
(dec9 10)

; 4. Write a function, mapset, that works like map except the return value is a
; set.
(defn mapset
  "Works like map except returns as set"
  [fn, lst]
  (set (map inc (set lst))))

(mapset inc [1 1 2 2])

; 5. Create a function that’s similar to symmetrize-body-parts except that it
; has to work with weird space aliens with radial symmetry. Instead of two
; eyes, arms, legs, and so on, they have five.
(defn penta-part
  [part]
  [{:name (clojure.string/replace (:name part) #"-1$" "-2")
    :size (:size part)}
   {:name (clojure.string/replace (:name part) #"-1$" "-3")
    :size (:size part)}
   {:name (clojure.string/replace (:name part) #"-1$" "-4")
    :size (:size part)}
   {:name (clojure.string/replace (:name part) #"-1$" "-5")
    :size (:size part)}])

(defn pentrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (reduce (fn [final-body-parts part]
            (into final-body-parts
                  (set (flatten [part (penta-part part)]))))
          []
          asym-body-parts))

(def alien-body-parts [{:name "head" :size 3}
                       {:name "eye-1" :size 1}
                       {:name "ear-1" :size 1}
                       {:name "mouth" :size 1}
                       {:name "nose" :size 1}
                       {:name "neck" :size 2}
                       {:name "shoulder-1" :size 3}
                       {:name "upper-arm-1" :size 3}
                       {:name "chest" :size 10}
                       {:name "back" :size 10}
                       {:name "forearm-1" :size 3}
                       {:name "abdomen" :size 6}
                       {:name "kidney-1" :size 1}
                       {:name "hand-1" :size 2}
                       {:name "knee-1" :size 2}
                       {:name "thigh-1" :size 4}
                       {:name "lower-leg-1" :size 3}
                       {:name "achilles-1" :size 1}
                       {:name "foot-1" :size 2}])

(clojure.pprint/pprint (pentrize-body-parts alien-body-parts))

; 6. Create a function that generalizes symmetrize-body-parts and the function
; you created in Exercise 5. The new function should take a collection of body
; parts and the number of matching body parts to add.
(defn recur-part
  "Given part-1, recursively generates part-n down to part-2."
  [n part]
  (if (= n 1)
    []
    [{:name (clojure.string/replace (:name part) #"-1$" (str "-" n))
      :size (:size part)} (recur-part (- n 1) part)]))

(defn n-part-maker
  "Create a custom part symmetrizer"
  [n]
  #(flatten (recur-part n %)))

(defn n-ize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [n asym-body-parts]
  (reduce (fn [final-body-parts part]
            (into final-body-parts
                  (set (flatten [part ((n-part-maker n) part)]))))
          []
          asym-body-parts))

(clojure.pprint/pprint (n-ize-body-parts 3 alien-body-parts))