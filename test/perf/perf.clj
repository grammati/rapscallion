(ns perf.perf
  (:require (rapscallion [core :as rap]))
  (:require (clojure [zip :as zip]))
  (:import [java.io Writer])
  )

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emitting XML

(defn ^String xml-escape [^String s]
  (-> s
      (.replaceAll "&"  "&amp;")
      (.replaceAll "<"  "&lt;")
      ))

(defn ^String xml-escape-attr [^String s]
  (-> s
      xml-escape
      (.replaceAll "'"  "&apos;")
      ))

(defn emit
  "Turn an element into XML"
                                        ;this mostly exists because clojure.xml/emit does not XML-escape text
  [elt]
  (let [sb (StringBuilder.)]
    (letfn [(p [& strs] (doseq [s strs] (.append sb s)))
            (write [elt]
              (if-not (map? elt)
                (p (xml-escape (str elt)))
                (let [{:keys [tag attrs content]} elt
                      tag (name tag)]
                  (p "<" tag)
                  (doseq [[k v] attrs]
                    (if-not (nil? v)
                      (p \space (name k) "='" (xml-escape-attr (str v)) \')))
                  (if (empty? content)
                    (p "/>")
                    (do 
                      (p ">")
                      (doseq [child content] (write child))
                      (p "</" tag ">"))))))]
      (write elt)
      (.toString sb))))

(defprotocol XMLWritable
  (emit-xml [this ^Writer out]))

(declare emit-element)

(defrecord Element [tag attrs content]
  XMLWritable
  (emit-xml [this out]
    (emit-element this out)))

(defn emit-element [^Element e ^Writer out]
  (let [{:keys [tag attrs content]} e]
    (doto out
      (.write "<")
      (.write (name tag)))
    (doseq [[k v] attrs]
      (if-not (nil? v)
        (doto out
          (.write " ")
          (.write (name k))
          (.write "='")
          (.write (xml-escape-attr (str v)))
          (.write "'"))))
    (if (empty? content)
      (.write out "/>")
      (do
        (.write out ">")
        (doseq [child content]
          (emit-xml child out))
        (doto out
          (.write "</")
          (.write (name tag))
          (.write ">"))))))

(def ^:dynamic *cdata-min-length* 256)

(defn long-string? [s]
  (> (count s) *cdata-min-length*))

(defn use-cdata? [s]
  (long-string? s))

(extend-protocol XMLWritable
  String
  (emit-xml [this out]))

(defn emit-proto [elt]
  (with-out-str
    (emit-xml elt *out*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zip vs. non-zip

(defn make-tree-rap [n]
  ((rap/template
    (str
     "<a rap:args='n' id='a'>"
     "<b rap:for='i (range n)' id='b$i'>"
     "<c rap:for='i (range n)' id='c$i'>"
     "<d rap:for='i (range n)' id='d$i'>"
     "<e rap:for='i (range n)' id='e$i'>"
     "</e>"
     "</d>"
     "</c>"
     "</b>"
     "</a>"
     ))
   {:n n}))

(defn make-tree-map [n]
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

(defn make-tree-elt [n]
  (letfn [(e [t a c] (Element. t a (vec c)))]
    (e :a {:id "a"}
       (for [i (range n)]
         (e :b {:id (str "b" i)}
            (for [i (range n)]
              (e :c {:id (str "c" i)}
                 (for [i (range n)]
                   (e :d {:id (str "d" i)}
                      (for [i (range n)]
                        (e :e {:id (str "e" i)} nil)))))))))))



(defn test-emit-speed
  ([] (test-emit-speed 9))
  ([n]
     (let [t (make-tree-elt n)
           _ (println "emit")
           a (time (count (emit t)))
           _(println "emit-proto")
           b (time (count (emit-proto t)))]
       (= a b))))



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

(defn make-tree-literal []
  {:tag :aaa
   :attrs nil
   :content
   [{:tag :b, :attrs nil,
     :content [[{:tag :c, :attrs nil, :content []}
                {:tag :c, :attrs nil, :content []}
                {:tag :c, :attrs nil, :content []}]]}
    {:tag :b, :attrs nil,
     :content [[{:tag :c, :attrs nil, :content []}
                {:tag :c, :attrs nil, :content []}
                {:tag :c, :attrs nil, :content []}]]}
    {:tag :b, :attrs nil,
     :content [[{:tag :c, :attrs nil, :content []}
                {:tag :c, :attrs nil, :content []}
                {:tag :c, :attrs nil, :content []}]]}]})


(def the-tree {:tag :aaa
               :attrs nil
               :content
               [{:tag :b, :attrs nil,
                 :content [[{:tag :c, :attrs nil, :content []}
                            {:tag :c, :attrs nil, :content []}
                            {:tag :c, :attrs nil, :content []}]]}
                {:tag :b, :attrs nil,
                 :content [[{:tag :c, :attrs nil, :content []}
                            {:tag :c, :attrs nil, :content []}
                            {:tag :c, :attrs nil, :content []}]]}
                {:tag :b, :attrs nil,
                 :content [[{:tag :c, :attrs nil, :content []}
                            {:tag :c, :attrs nil, :content []}
                            {:tag :c, :attrs nil, :content []}]]}]})

(defn make-tree-global []
  the-tree)
