(ns rapscallion.xml
  (:require (clojure [xml :as xml]))
  )


(defn parse
  "Like clojure.xml/parse, but also accepts a string containing XML."
  [in]
  (let [in (if (and (string? in) (.startsWith in "<"))
        (-> in .getBytes java.io.ByteArrayInputStream.)
        in)]
    (clojure.xml/parse in)))


 
(defn element
  "Factory function to return an element."
  ([tag] (element tag nil nil))
  ([tag attrs] (element tag attrs nil))
  ([tag attrs content] (struct xml/element tag attrs content)))

(defn element?
  "Returns true if the given object is an element."
  [e]
  (and (map? e) (:tag e)))

(def element-type clojure.lang.IPersistentMap)


(defn get-attr [elt attr]
  (get-in elt [:attrs attr]))

(defn assoc-attr [elt & attr-vals]
  (assoc elt :attrs (apply assoc (:attrs elt) attr-vals)))

(defn merge-attrs [elt new-attrs]
  (assoc elt :attrs (merge (:attrs elt) new-attrs))) ;TODO - skip nils

(defn dissoc-in [m ks]
  (let [k (last ks)
        ks (butlast ks)]
    (if ks
      (if-let [mm (get-in m ks)]
        (assoc-in m ks (dissoc mm k))
        m)
      (if k
        (dissoc m k)
        m))))

(defn dissoc-attr [elt & attrs]
  (assoc elt :attrs (apply dissoc (:attrs elt) attrs)))

(defn pop-attr [elt attr]
  [(dissoc-attr elt attr) (get-attr elt attr)])

(defn pop-attrs [elt & attrs]
  (loop [elt elt
         [attr & attrs] attrs
         vals []]
    (if attr
      (let [[elt val] (pop-attr elt attr)]
        (recur elt attrs (conj vals val)))
      [elt vals])))

(defn append-content [elt & children]
  (update-in elt [:content] concat children))

(defn xml-escape [#^String s]
  (-> s
      (.replaceAll "&"  "&amp;")
      (.replaceAll "<"  "&lt;")
      ))

(defn xml-escape-attr [#^String s]
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

