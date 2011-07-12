(ns perf.perf
  (:require (rapscallion [core :as rap]))
  (:require (clojure [zip :as zip]))
  )

(defn make-tree-2 [n]
  ((rap/template
    (str
     "<a rap:args='n'>"
     "<b rap:for='_ (range n)'>"
     "<c rap:for='_ (range n)'>"
     "<d rap:for='_ (range n)'>"
     "<e rap:for='i (range n)' id='$i'>"
     "</e>"
     "</d>"
     "</c>"
     "</b>"
     "</a>"
     ))
   {:n n}))

(defn make-tree [n]
  {:tag :a
   :content
   (vec (for [_ (range n)]
          {:tag :b
           :content
           (vec (for [_ (range n)]
                  {:tag :c
                   :content
                   (vec (for [_ (range n)]
                          {:tag :d
                           :content
                           (vec (for [i (range n)]
                                  {:tag :e
                                   :attrs {:id (str i)}}))}))}))}))})

(defn modify-nonzip [tree]
  (let [n (count (:content tree))]
    (loop [tree tree
           i 0]
     (if (= i n)
       tree
       (recur (assoc-in tree [:content 0 ;first b
                              :content 0 ;first c
                              :content 0 ;first d
                              :content i ;i-th e
                              :attrs   :name
                              ] (str "name-" i))
              (inc i))))))

(defn modify-zip [tree]
  (let [first-e (-> tree zip/xml-zip zip/down zip/down zip/down zip/down)]
    (loop [e first-e
           i 0]
      (let [e (zip/edit e assoc-in [:attrs :name] (str "name-" i))]
        (if-let [next-e (zip/right e)]
          (recur next-e (inc i))
          (zip/root e))))))
