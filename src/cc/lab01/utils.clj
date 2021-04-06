(ns cc.lab01.utils
  (:require [clojure.set]))

(defn graph->dot [graph]
  (let [dotstring "digraph FSA {\n"
        dotstring (reduce (fn [text state]
                            (str text
                                 (format "\"start %s\" " state)
                                 (format "[label=\"start %s\" shape=none height=0 width=0]\n" state)
                                 (format "\"start %s\" -> \"%s\"\n" state state)))
                          dotstring
                          (:start graph))
        dotstring (reduce (fn [text state]
                            (format "%s\"%s\" [shape=doublecircle]\n" text state))
                          dotstring
                          (:finish graph))
        dotstring (reduce (fn [text state]
                            (format "%s\"%s\" [shape=circle]\n" text state))
                          dotstring
                          (clojure.set/difference (:states graph) (:finish graph)))
        dotstring (reduce-kv (fn [text start symboled-endings]
                               (let [lines (for [[sym endings] symboled-endings
                                                 finish endings]
                                             (format "\"%s\" -> \"%s\" [label=%s]\n"
                                                     start
                                                     finish
                                                     (if (= :null sym) "ε" sym)))]
                                 (apply str text lines)))
                             dotstring
                             (:transitions graph))]
    (str dotstring "}")))
