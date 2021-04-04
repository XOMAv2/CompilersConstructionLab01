(ns cc.lab01.regex-parser
  (:require [cc.lab01.expressions :as ex]))

(declare parse
         parse-alt
         parse-succ
         parse-unary
         parse-brackets
         parse-sym
         parse-null)

(def debug? true)
(def unary-ops (set "+*?"))
(def binary-ops (set "|"))

(defn- balanced?
  "Если круглые скобки рассталены правильно, возвращает true, иначе false."
  [input]
  (empty? (reduce (fn [stack c] (case c
                                  \( (conj stack c)
                                  \) (if (seq stack)
                                       (pop stack)
                                       (reduced [:UNDERFLOW]))
                                  stack))
                  '()
                  input)))

(defn- get-depth-vec
  "Возвращает вектор, в котором каждому символу из входной строки s соответствует число,
   характеризующее текущую глубину вложенности круглых скобок.
   Пример: \"(ab()))(\" -> [1 1 1 2 1 0 -1 0]"
  [s]
  (->> s
       (map #(case % \( 1 \) -1 0))
       (reductions +)
       (vec)))

(defn- indexes-of
  "Возвращает индексы символа value (символ или строка из одного символа) в s. Если value не
   найдено, будет возвращена пустая коллекция."
  ([s value]
   (->> s
        (map-indexed (fn [index c]
                       (when (= (str c) (str value))
                         index)))
        (filter some?)))
  ([s v1 & values]
   (->> (conj values v1)
        (set)
        (map #(indexes-of s %))
        (apply concat)
        (sort))))

(defn parse [input]
  (when (not (balanced? input))
    (throw (Exception. "Не удалось построить КА по введённой строке.")))
  (if-let [graph (parse-alt input)]
    (do (when debug? (println "alt"))
        graph)
    (if-let [graph (parse-succ input)]
      (do (when debug? (println "succ"))
          graph)
      (if-let [graph (parse-unary input)]
        (do (when debug? (println "unary"))
            graph)
        (if-let [graph (parse-brackets input)]
          (do (when debug? (println "brackets"))
              graph)
          (if-let [graph (parse-sym input)]
            (do (when debug? (println "sym"))
                graph)
            (if-let [graph (parse-null input)]
              (do (when debug? (println "null"))
                  graph)
              (throw (Exception. "Не удалось построить КА по введённой строке.")))))))))

(defn- parse-alt [input]
  (when (> (count input) 2)
    (let [depth-vec (get-depth-vec input)
          indexes (indexes-of input "|")]
      (->> indexes
           (filter #(zero? (depth-vec %)))
           (first)
           ((fn [index]
              (when (some? index)
                (ex/alt (parse (subs input 0 index))
                        (parse (subs input (inc index)))))))))))

(defn- parse-succ [input]
  (when (> (count input) 1)
    (let [depth-vec (get-depth-vec input)]
      (->> input
           (partition 2 1)
           (map-indexed #(vector % %2))
           (filter (fn [[i [a b]]] (and (not (binary-ops a))
                                        (not (binary-ops b))
                                        (not (unary-ops b))
                                        (zero? (depth-vec i)))))
           (first)
           (first)
           ((fn [index]
              (when (some? index)
                (ex/succ (parse (subs input 0 (inc index)))
                         (parse (subs input (inc index)))))))))))

(defn- parse-unary [input]
  (when (> (count input) 1)
    (when-let [sym (last input)]
      (when-let [_ (unary-ops sym)]
        (when-let [_ (zero? (last (get-depth-vec input)))]
          (let [substr (apply str (butlast input))]
            (case sym
              \* (ex/star (parse substr))
              \+ (ex/plus (parse substr))
              \? (ex/opt (parse substr))
              nil)))))))

(defn- parse-brackets [input]
  (when (> (count input) 1)
    (when-let [_ (balanced? input)]
      (when-let [_ (and (= (first input) \()
                        (= (last input) \)))]
        (parse (apply str (butlast (rest input))))))))

(defn- parse-sym [input]
  (when (= (count input) 1)
    (when-let [_ (not (unary-ops (first input)))]
      (when-let [_ (not (binary-ops (first input)))]
        (ex/sym input)))))

(defn- parse-null [input]
  (when (empty? input)
    (ex/null)))
