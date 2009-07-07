(ns ws (:gen-class))

(load-file "utility.clj")
(refer 'utility :only '(assure i))

(defn makempty [width height]
  (reduce #(assoc %1 %2 nil) [] (range (i width * height))))

(defn randletter []
  (nth "abcdefghijklmnopqrstuvwxyz" (rand-int 26)))

(defn wordsearch [width height words]
  (let [xdirs [-1 0 1]
	ydirs [(- width) 0 width]
	randdirs (fn []
		   (assure (apply distinct? it) [(xdirs (rand-int 3))
						 (ydirs (rand-int 3))]))
	add-word (fn [search word]
		   (let [ind (rand-int (i width * height - 1))
			 dirs (randdirs)
			 dir (apply + dirs)
			 offset (i (mod ind width) + (count word) * (dirs 0))
			 lastind (i ind + dir * (count word))]
		     (if (and (i lastind > -1)
			      (i lastind < width * height)
			      (i offset > -1)
			      (i offset < width))
		       (reduce (fn [search pair]
				 (if search
				   (aif (search (pair 0))
					(if (= it (pair 1))
					  search
					  nil)
					(assoc search (pair 0) (pair 1)))
				   nil))
			       search
			       (map vector (iterate #(+ % dir) ind) word))
		       nil)))]
    (map (partial apply str)
	 (partition width (map #(if % % (randletter))
			       (reduce #(assure (add-word %1 %2))
				       (makempty width height)
				       words))))))

(defn printws [search]
  (do (dorun (map println search)) nil))
