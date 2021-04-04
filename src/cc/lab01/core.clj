(ns cc.lab01.core
  (:require [cc.lab01.expressions :as ex]
            [cc.lab01.morph :refer [nfa->dfa]]
            [cc.lab01.utils :refer [graph->dot]])
  (:gen-class))

(def graph (ex/succ (ex/alt (ex/sym "a")
                            (ex/sym "b")
                            (ex/sym "c")
                            (ex/null))
                    (ex/sym "c")))

(spit "graph.dot" (graph->dot (nfa->dfa graph)))

(spit "graph.dot" (graph->dot graph))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
