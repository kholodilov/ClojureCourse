(defn f ^{ :pre [(pos? x)] :post [(< 10 % 20)] } [x & rest] (println "fff") (+ 1 x))

(let [g (fn [x] (inc x))] 
    (g 2))

#(+ %1 %2)

(#(str %1 "-" %&) 1 2 3 4)

(apply + 1 2 [3 4 5])
(reduce + 0 [1 2 3 4 5])

(let [f (fn [x] (str (vec (reverse x))))
      g (fn [x] ((comp str vec reverse) x))]
    (println (f (list 1 2 3)))
    (println (g (list 1 2 3))))

(defn g [x y z] (+ x y z))
((partial g 1 2) 3)

(identity 7)
((constantly 7) 1 2 3)

(let [xs [1 2 3 4 5]]
    (map (juxt dec identity inc) xs))