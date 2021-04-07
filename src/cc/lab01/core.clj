(ns cc.lab01.core
  (:require [cc.lab01.regex-parser :refer [parse]]
            [cc.lab01.morph :as morph]
            [cc.lab01.utils :refer [graph->dot]])
  (:gen-class))

(defn -main
  [& args]
  (try (let [nfa (parse (first args))
             _   (spit "graph-nfa.dot" (graph->dot nfa))
             _   (spit "graph-nfa-reverse.dot" (graph->dot (morph/graph-reverse nfa)))
             dfa (morph/nfa->dfa nfa)
             _   (spit "graph-dfa.dot" (graph->dot dfa))
             _   (spit "graph-dfa-reverse.dot" (graph->dot (morph/graph-reverse dfa)))
             minimal (morph/minimize nfa)
             _       (spit "graph-minimal.dot" (graph->dot minimal))
             _       (spit "graph-minimal-simple.dot" (graph->dot minimal :simple-names true))])
       (catch Exception e
         (println (ex-message e)))))

#_(-main "(a|b*)+")
#_(-main "a|b")
#_(-main "a(bc?d|e+)*")
