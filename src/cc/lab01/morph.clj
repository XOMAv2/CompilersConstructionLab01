(ns cc.lab01.morph
  (:require [cc.lab01.utils :refer [final?]]))

(comment "В мапе переходов transitions детерминированного конечного автомата каждое множество
          конечных состояний симвализируют лишь одно состояние в ДКА.")

(declare null-closure)
(declare alpha-null-closure)
(declare nfa->dfa)

(defn null-closure [state nfa]
  (loop [transitions (:transitions nfa)
         closure #{state}
         checkpoints #{state}]
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
                (map #(null-closure % nfa)
                     alpha-states)))))

(defn nfa->dfa [nfa]
  (loop [queue (list (null-closure (:start nfa) nfa))
         alphabet (:alphabet nfa)
         dfa-states #{}
         dfa-transitions {}]
    (if (empty? queue)
      {:graph-type :DFA
       :start (null-closure (:start nfa) nfa)
       :finish (set (filter #(final? %) dfa-states))
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
                                                  (second %2))
                                       dfa-transitions
                                       entries)]
            (recur queue alphabet dfa-states dfa-transitions)))))))