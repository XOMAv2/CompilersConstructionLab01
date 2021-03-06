(ns cc.lab01.morph
  (:require [clojure.set]))

(declare null-closure)
(declare alpha-null-closure)
(declare nfa->dfa)

(defn null-closure [states nfa]
  (loop [transitions (:transitions nfa)
         closure states
         checkpoints states]
    (let [next-checkpoints (apply concat
                                  (map #(get-in transitions [% :null])
                                       checkpoints))
          next-closure (into closure next-checkpoints)]
      (if (= closure next-closure)
        closure
        (recur transitions
               next-closure
               (set next-checkpoints))))))

(defn alpha-null-closure [states nfa c]
  (let [transitions (:transitions nfa)
        alpha-states (set (apply concat
                                 (map #(get-in transitions [% c])
                                      states)))]
    (set (apply concat
                (map #(null-closure #{%} nfa)
                     alpha-states)))))

(defn nfa->dfa [nfa]
  (loop [queue (list (null-closure (:start nfa) nfa))
         alphabet (:alphabet nfa)
         dfa-states #{}
         dfa-transitions {}]
    (if (empty? queue)
      {:graph-type :DFA
       :start #{(null-closure (:start nfa) nfa)}
       :finish (->> dfa-states
                    (filter #(seq (clojure.set/intersection (:finish nfa) %)))
                    (set))
       :alphabet alphabet
       :states dfa-states
       :transitions dfa-transitions}
      (let [state (peek queue)
            queue (pop queue)]
        (if (contains? dfa-states state)
          (recur queue alphabet dfa-states dfa-transitions)
          (let [entries (for [c alphabet]
                          (list c (alpha-null-closure state nfa c)))
                entries (filter #(not (empty? (second %))) entries)
                queue (into queue (map second entries))
                dfa-states (conj dfa-states state)
                dfa-transitions (reduce #(assoc-in %
                                                  [state (first %2)]
                                                  #{(second %2)})
                                       dfa-transitions
                                       entries)]
            (recur queue alphabet dfa-states dfa-transitions)))))))

(defn graph-reverse [graph]
  (let [graph (assoc graph
                     :start (:finish graph)
                     :finish (:start graph))
        triads (for [i (-> graph :transitions keys)
                     j (-> graph :transitions (get i) keys)
                     k (-> graph :transitions (get i) (get j))]
                 [k j i])
        r-transitions (reduce (fn [acc [k j i]]
                                (update-in acc [k j] (comp set conj) i))
                              {}
                              triads)]
    (assoc graph :transitions r-transitions)))

(defn minimize
  "?????????????????????? ???? ?? ???????????????????????????? ?????????????????? ????????????????????????."
  [graph]
  (-> graph
      (graph-reverse)
      (nfa->dfa)
      (graph-reverse)
      (nfa->dfa)))
