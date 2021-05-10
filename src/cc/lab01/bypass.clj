(ns cc.lab01.bypass)

(declare accepts?)
(declare nfa-accepts?)
(declare dfa-accepts?)

(defmulti accepts?
  (fn [fsa _] (:graph-type fsa)))

(defmethod accepts? :NFA [nfa stream]
  (nfa-accepts? nfa stream))

(defmethod accepts? :DFA [dfa stream]
  (dfa-accepts? dfa stream))

(defn nfa-accepts? [nfa stream]
  {:pre [(= (count (:start nfa)) 1)]}
  (throw (Exception. "Not implemented.")))

;; (defn nfa-accepts? [nfa stream]
;;   {:pre [(= (count (:start nfa)) 1)
;;          (->> stream
;;               (map #((:alphabet nfa) (str %)))
;;               (reduce #(and % %2) true)
;;               (boolean))]}
;;   (loop [queue (list [(first (:start nfa)) stream])]
;;     (println queue)
;;     (if (empty? queue)
;;       false
;;       (let [[cur-state cur-stream] (peek queue)
;;             queue (pop queue)]
;;         (if (empty? cur-stream)
;;           (or (-> cur-state ((:finish nfa)) (some?))
;;               (recur (into queue
;;                            (map #(vector % cur-stream)
;;                                 (get-in nfa [:transitions cur-state :null])))))
;;           (recur (-> queue
;;                      (into (map #(vector % cur-stream)
;;                                 (get-in nfa [:transitions cur-state :null])))
;;                      (into (map #(vector % (rest cur-stream))
;;                                 (get-in nfa [:transitions
;;                                              cur-state
;;                                              (str (first cur-stream))]))))))))))

;; (->> (:transitions dfa)
;;      (vals)
;;      (reduce #(into % (vals %2)) [])
;;      (some #(> (count %) 1))
;;      (not))

(defn dfa-accepts? [dfa stream]
  {:pre [(= (count (:start dfa)) 1)]}
  (doseq [i (-> dfa :transitions keys)
          j (-> dfa :transitions (get i) keys)]
    (when (-> dfa :transitions (get i) (get j) (count) (not= 1))
      (throw (Exception. "ДКА должен содержать только одно начальное состояние."))))
  (loop [cur-state (first (:start dfa))
         cur-stream stream]
    (if (empty? cur-stream)
      (-> cur-state ((:finish dfa)) (some?))
      (let [token (str (first cur-stream))
            next-state (first (get-in dfa [:transitions cur-state token]))
            next-stream (rest cur-stream)]
        (if (some? next-state)
          (recur next-state next-stream)
          false)))))