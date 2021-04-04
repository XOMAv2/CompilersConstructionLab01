(ns cc.lab01.utils)

(defn final? [state]
  (if (set? state)
    (reduce #(or % @%2) false state)
    @state))

