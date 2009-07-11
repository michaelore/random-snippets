;Generation and mutation of elementary functions
(ns genetic)

(load-file "utility.clj")
(refer 'utility :only '(pick signum))

(defn +- []
  (signum (- (rand) 0.5)))

(defn rand-num []
  (if (> (rand) 0.5)
    (* 10 (rand-num) (+-))
    (rand)))

(defn wrap [t x]
  {:type t :val x})

(def basics (concat '[Math/sin Math/sin
		      Math/exp Math/log
		      Math/sqrt]
		    (replicate 3 'identity)))

(def ops (map #(wrap :Op %) '(+ - * /)))

(defn get-fn []
  (wrap :Fn (pick basics)))

(defn get-node []
  (wrap :Node (pick ['x (rand-num)])))

(defn combine [foo bar]
  (pick (map #(list % foo bar) ops)))

(defn elem-fn []
  (wrap :List (list (get-fn) (get-node))))

(defmulti compile-multi :type)

(defmethod compile-multi :Fn [f]
  (f :val))

(defmethod compile-multi :Node [n]
  (n :val))

(defmethod compile-multi :List [x]
  (map compile-multi (x :val)))

(defmethod compile-multi :Op [o]
  (o :val))

(defn compile-elem [e]
  (eval (list 'fn ['x] (compile-multi e))))

(defmulti mutate :type)

(defmethod mutate :Fn [f]
  (if (> (rand) 0.8)
    (get-fn)
    f))

(defn mutate-scalar [n]
  (let [num (rand)]
    (cond (> num 0.9) (wrap :List (list (get-fn) n))
	  (> num 0.85) (wrap :List (combine (elem-fn) n))
	  (> num 0.8) (wrap :List (combine n (elem-fn)))
	  (= 1 1) n)))

(defmethod mutate :Node [n]
  (mutate-scalar n))

(defmethod mutate :List [x]
  (mutate-scalar (wrap :List (map mutate (x :val)))))

(defmethod mutate :Op [o]
  (if (> (rand) 0.8)
    (pick ops)
    o))

(defn testit [n]
  (compile-multi (nth (iterate mutate (elem-fn)) n)))
