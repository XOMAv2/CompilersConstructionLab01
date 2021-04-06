(ns cc.lab01.core
  (:require [cc.lab01.regex-parser :refer [parse]]
            [cc.lab01.morph :refer [nfa->dfa graph-reverse]]
            [cc.lab01.utils :refer [graph->dot]])
  (:gen-class))

(defn -main
  [& args]
  (try (let [nfa (parse (first args))
             _   (spit "graph-nfa.dot" (graph->dot nfa))
             _   (spit "graph-nfa-reverse.dot" (graph->dot (graph-reverse nfa)))
             dfa (nfa->dfa nfa)
             _   (spit "graph-dfa.dot" (graph->dot dfa))
             _   (spit "graph-dfa-reverse.dot" (graph->dot (graph-reverse dfa)))])
       (catch Exception e
         (println (ex-message e)))))

#_(-main "(a|b*)+")
#_(-main "a|b")
