(ns cc.lab01.utils
  (:require [clojure.set]))

(defn final? [state]
  (if (set? state)
    (reduce #(or % @%2) false state)
    @state))

(defn graph->dot [graph]
  (let [kvs (map-indexed (fn [index state]
                           [state (str index)])
                         (:states graph))
        names (apply conj {} kvs)
        dotstring (str "digraph FSA {\n"
                       "start [label=start shape=none height=0 width=0]\n"
                       "start -> " (names (:start graph)) "\n")
        finish-states (if (= (:graph-type graph) :DFA)
                        (:finish graph)
                        #{(:finish graph)})
        dotstring (reduce (fn [text state]
                            (str text (names state) " [shape=doublecircle]\n"))
                          dotstring
                          finish-states)
        dotstring (reduce (fn [text state]
                            (str text (names state) " [shape=circle]\n"))
                          dotstring
                          (clojure.set/difference (:states graph) finish-states))
        dotstring (reduce-kv (fn [text start symboled-endings]
                               (let [lines (for [[sym endings] symboled-endings
                                                 :let [endings (if (= (:graph-type graph) :DFA)
                                                                 #{endings}
                                                                 endings)]
                                                 finish endings]
                                             (format "%s -> %s [label=%s]\n"
                                                     (names start)
                                                     (names finish)
                                                     (if (= :null sym) "ε" sym)))]
                                 (apply str text lines)))
                             dotstring
                             (:transitions graph))]
    (str dotstring "}")))