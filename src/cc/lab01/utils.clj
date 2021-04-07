(ns cc.lab01.utils
  (:require [clojure.set]))

(defn graph->dot [graph & {:keys [simple-names]}]
  (let [names (if simple-names
                (->> (:states graph)
                     (map-indexed #(vector %2 %))
                     (apply conj {}))
                (:states graph))
        dotstring "digraph FSA {\n"
        dotstring (reduce (fn [text state]
                            (str text
                                 (format "\"start %s\" " (names state))
                                 (format "[label=\"start %s\" shape=none height=0 width=0]\n" (names state))
                                 (format "\"start %s\" -> \"%s\"\n" (names state) (names state))))
                          dotstring
                          (:start graph))
        dotstring (reduce (fn [text state]
                            (format "%s\"%s\" [shape=doublecircle]\n" text (names state)))
                          dotstring
                          (:finish graph))
        dotstring (reduce (fn [text state]
                            (format "%s\"%s\" [shape=circle]\n" text (names state)))
                          dotstring
                          (clojure.set/difference (:states graph) (:finish graph)))
        dotstring (reduce-kv (fn [text start symboled-endings]
                               (let [lines (for [[sym endings] symboled-endings
                                                 finish endings]
                                             (format "\"%s\" -> \"%s\" [label=%s]\n"
                                                     (names start)
                                                     (names finish)
                                                     (if (= :null sym) "ε" sym)))]
                                 (apply str text lines)))
                             dotstring
                             (:transitions graph))]
    (str dotstring "}")))
