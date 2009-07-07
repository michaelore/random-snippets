(ns utility (:gen-class))

(defmacro aif [cond then else]
  "like if but binds cond to 'it"
  `(let [~'it ~cond]
     (if ~'it
       ~then
       ~else)))

(defn max-ind [coll]
  "the index of the greatest number in a coll"
  (loop [coll* coll ind 0 record 0 record-ind 0]
    (if (empty? coll*)
      record-ind
      (recur (rest coll*)
	     (+ ind 1)
	     (if (> (first coll*) record) (first coll*) record)
	     (if (> (first coll*) record) ind record-ind)))))

(defn every-other [coll] ;returns a vector
  "every other element of a coll"
  (loop [coll* coll ind 0 acc []]
    (if (empty? coll*)
      acc
      (if (== (rem ind 2) 0)
	(recur (rest coll*) (+ 1 ind) acc)
	(recur (rest coll*) (+ 1 ind) (conj acc (first coll*)))))))

(def precedence (hash-map))

(defn makeinfix [op prec]
  "makes op an infix operator with precedence prec"
  (def precedence (assoc precedence op prec)))

(defmacro i [& elements]
  "macro for infix operators: converts (i 5 * 4 + 3) to (+ (* 5 4) 3)"
  (loop [s-exp elements]
    (let [ops (every-other s-exp)]
      (if (empty? ops)
	(first s-exp)
	(let [opind (+ 1 (* 2 (max-ind (map precedence ops))))
	      new-exp (list (nth s-exp opind) (nth s-exp (- opind 1)) (nth s-exp (+ opind 1)))]
	  (recur (concat (take (- opind 1) s-exp) (list new-exp) (drop (+ opind 2) s-exp))))))))

(makeinfix '* 12)
(makeinfix '/ 12)
(makeinfix '+ 11)
(makeinfix '- 11)
(makeinfix '> 9)
(makeinfix '< 9)
(makeinfix '>= 9)
(makeinfix '<= 9)
(makeinfix '= 8)

(defmacro assure
  "Repeatedly evaluates x and binds the result to 'it until pred is true"
  ([pred x]
     `(loop []
	(let [~'it ~x]
	  (if ~pred
	    ~'it
	    (recur)))))
  ([x]
     `(assure (identity ~'it) ~x)))

(defn signum [n]
  "the sign of a number"
  (if (pos? n)
    1
    (if (neg? n)
      -1
      0)))