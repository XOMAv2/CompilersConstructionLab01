(ns cc.lab01.core
  (:require [cc.lab01.regex-parser :refer [parse]]
            [cc.lab01.morph :as morph]
            [cc.lab01.utils :refer [graph->dot]]
            [cc.lab01.bypass :refer [accepts?]])
  (:gen-class))

(defn -main
  [regex input & args]
  (try (let [nfa (parse regex)
             _   (spit "graph-nfa.dot" (graph->dot nfa))
             _   (spit "graph-nfa-reverse.dot" (graph->dot (morph/graph-reverse nfa)))
             dfa (morph/nfa->dfa nfa)
             _   (spit "graph-dfa.dot" (graph->dot dfa))
             _   (spit "graph-dfa-reverse.dot" (graph->dot (morph/graph-reverse dfa)))
             minimal (morph/minimize nfa)
             _       (spit "graph-minimal.dot" (graph->dot minimal))
             _       (spit "graph-minimal-simple.dot" (graph->dot minimal :simple-names true))]
         (println (format "The string \"%s\" %s the regular expression \"%s\"."
                          input
                          (if (accepts? minimal input) "matches" "does not match")
                          regex)))
       (catch Exception e
         (println (ex-message e)))))

#_(-main "(a|b*)+" "bbbbbaac")
#_(-main "a|b" "b")
#_(-main "a(bc?d|e+)*" "c")
