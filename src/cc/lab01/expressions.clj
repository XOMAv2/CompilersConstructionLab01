(ns cc.lab01.expressions
  (:require [clojure.set]))

(declare null)
(declare sym)
(declare alt)
(declare succ)
(declare star)
(declare plus)
(declare opt)
(declare rng)

(defn null
  "Возвращает НКА, который принимает только пустую последовательность."
  []
  (let [start (gensym "s")
        finish (gensym "s")]
    {:graph-type :NFA
     :start #{start}
     :finish #{finish}
     :alphabet #{}
     :states #{start finish}
     :transitions {start {:null #{finish}}}}))

(defn sym
  "Возвращает НКА, который принимает только s, где s может быть строкой или одним символом."
  [s]
  (let [start (gensym "s")
        finish (gensym "s")]
    {:graph-type :NFA
     :start #{start}
     :finish #{finish}
     :alphabet #{(str s)}
     :states #{start finish}
     :transitions {start {(str s) #{finish}}}}))

(defn simplify [graph]
  (when (not= (count (:start graph)) 1)
    (throw (Exception.
            (str "Число начальных состояний в конструкторе Томпсона должно быть равно единице."))))
  (when (not= (count (:finish graph)) 1)
    (throw (Exception.
            (str "Число конечных состояний в конструкторе Томпсона должно быть равно единице."))))
  (let [graph (update graph :start first)
        graph (update graph :finish first)]
    graph))

(defn alt
  "Возвращает НКА, который принимает всё, что принимается любым из графов graphs. Ведет себя как
   (null) если подграфы не заданы."
  ([] (null))
  ([& graphs]
   (let [graphs (map simplify graphs)
         start (gensym "s")
         finish (gensym "s")
         alphabet (apply clojure.set/union (map :alphabet graphs))
         states (apply clojure.set/union (map :states graphs))
         states (into #{start finish} states)
         transitions (apply merge (map :transitions graphs))
         transitions (reduce (fn [transitions graph]
                               (update-in transitions
                                          [start :null]
                                          (comp set conj) (:start graph)))
                             transitions
                             graphs)
         transitions (reduce (fn [transitions graph]
                               (update-in transitions
                                          [(:finish graph) :null]
                                          (comp set conj) finish))
                             transitions
                             graphs)]
     {:graph-type :NFA
      :start #{start}
      :finish #{finish}
      :alphabet alphabet
      :states states
      :transitions transitions})))

(defn succ
  "Возвращает НКА, который принимает всё, что принимается графами graphs выстроенными
   последовательно. Ведет себя как (null), если подграфы не заданы."
  ([] (null))
  ([& graphs]
   (let [graphs (map simplify graphs)
         start (gensym "s")
         finish (gensym "s")
         alphabet (apply clojure.set/union (map :alphabet graphs))
         states (apply clojure.set/union (map :states graphs))
         states (into #{start finish} states)
         transitions (apply merge (map :transitions graphs))
         pairs (partition 2 1 graphs)
         transitions (reduce (fn [transitions pair]
                               (let [f (:finish (first pair))
                                     s (:start (second pair))]
                                 (update-in transitions
                                            [f :null]
                                            (comp set conj) s)))
                             transitions
                             pairs)
         transitions (update-in transitions
                                [start :null]
                                (comp set conj) (:start (first graphs)))
         transitions (update-in transitions
                                [(:finish (last graphs)) :null]
                                (comp set conj) finish)]
     {:graph-type :NFA
      :start #{start}
      :finish #{finish}
      :alphabet alphabet
      :states states
      :transitions transitions})))

(defn star
  "Возвращает НКА, который принимает все, что принимается графом НКА ноль или более раз."
  [graph]
  (let [graph (simplify graph)
        start (gensym "s")
        finish (gensym "s")
        alphabet (:alphabet graph)
        states (into #{start finish} (:states graph))
        transitions (:transitions graph)
        transitions (update-in transitions
                               [start :null]
                               (comp set conj) (:start graph))
        transitions (update-in transitions
                               [(:finish graph) :null]
                               (comp set conj) finish)
        transitions (update-in transitions
                               [start :null]
                               (comp set conj) finish)
        transitions (update-in transitions
                               [(:finish graph) :null]
                               (comp set conj) (:start graph))]
    {:graph-type :NFA
     :start #{start}
     :finish #{finish}
     :alphabet alphabet
     :states states
     :transitions transitions}))

(defmacro plus
  "Возвращает НКА, который принимает все, что принимается графом НКА один или несколько раз."
  [graph]
  `(succ ~graph
         (star ~graph)))

(defmacro opt
  "Возвращает НКА, который принимает все, что принимается графом НКА или пустой
   последовательностью."
  [graph]
  `(alt (null)
        ~graph))

(defmacro rng
  "Возвращает НКА, который принимает все, что принимается графом НКА, последовательно
   реплицированным от lower до upper (включительно) раз.
   Если верхняя граница не зададна, то граф будет реплецирован ровно lower раз.
   Если верхняя граница равна :inf, то НКА принимает все, что принимается графом, реплицированным по
   крайней мере lower раз подряд."
  ([graph n]
   `(succ ~@(repeat n graph)))
  ([graph lower upper]
   (if (= upper :inf)
     `(succ ~@(repeat lower graph) (star ~graph))
     `(alt ~@(map #(apply list `succ (repeat % graph))
                  (range lower (+ upper 1)))))))
