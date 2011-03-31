(ns rapscallion.text
  (:import (java.io StringReader))
  (:use [clojure.walk :only [postwalk-replace]])
  (:use [clojure.string :only [triml]])
  (:use [clojure.java.io :only [reader]])
  (:use [rapscallion.core :only [extract-exprs read-all flatseq]])
  )

(def ^{:dynamic true} *code-line-marker* "%")

(defn compile-meta [line meta]
  (let [line (triml line)
        [directive rest] (.split #"\s+" line 2)]
    (case directive
      "args" (assoc meta :args (read-all rest))
      (throw (IllegalArgumentException. (str "Unknown directive " directive))))))

(defn compile-template [s]
  (let [s (if (string? s) (-> s StringReader. reader) s)
        lines (line-seq s)
        X *code-line-marker*
        special-line? (fn [line]
                        (.startsWith (triml line) (str X X)))
        code-line? (fn [line]
                     (and (.startsWith (triml line) X) (not (special-line? line))))
        text-line? #(not (code-line? %))
        ]
    (loop [[line & more :as lines] lines
           subs {}
           meta {}
           parts []]
      (cond
        (nil? line)
          ; we have consumed all the lines - finish compiling.
          (let [compiled (->> parts
                              (interpose " ")
                              (apply str)
                              read-all
                              (postwalk-replace subs))]
            (eval `(fn [{:keys [~@(:args meta)]}] (apply str (flatseq ~@compiled)))))
        (special-line? line)
          ; special directive, such as %%args foo bar
          (recur more subs (compile-meta (.substring (triml line) 2) meta) parts)
        (code-line? line)
          ; code line, such as %(for [foo bar]
          (recur more subs meta (conj parts (.substring (triml line) 1)))
        line
          ; any other line of text - pull out all adjacent text-lines,
          ; extract substitutions from the text, and store for later
          (let [[text-lines more] (split-with text-line? lines)
                text (apply str (concat (interpose \newline text-lines) "\n"))
                chunk (vec (extract-exprs text))
                sym (gensym)
                subs (assoc subs sym chunk)]
            (recur more subs meta (conj parts sym)))))))

