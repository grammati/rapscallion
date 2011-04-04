(ns rapscallion.match
  (:use (rapscallion
         (core :only [read-partial flatseq])))
  (:require (rapscallion
             (xml :as xml)))
  (:require (imparsonate
             (core :as imp)
             (lib :as implib))))

(imp/defparser match-expr-parser
  :root           :paths
  :paths          (imp/list-of :path "|")
  :path           #{:relpath :abspath}
  :abspath        ["/" :relpath]
  :relpath        (imp/list-of :elt "/")
  
  :elt            [#{:tag "*"} :filter*]
                    (fn [tag filters] {:tag tag :filters filters})
  
  :tag            #"\w+"
                    keyword
  
  :filter         #{:xml-filter :clj-filter}
  :attr-filter    ["@" :attrname ["=" :quoted-string]:?]
                    (fn [n v] {:type :attr :name n :value v})
                    
  :attrname       #"\w+" ;FIXME
  :quoted-string  implib/double-quoted-string
  
  
  :xml-filter     ["[" :attr-filter "]"]
  
  :clj-filter     ["{" :clj-form "}"]
                    (fn [form] {:type :expr :form form})
                    
  :clj-form       read-partial
  
  )


(defmulti element-filter :type)

(defmethod element-filter :attr [m]
  (let [attr  (keyword (:name m))
        value (:value m)]
    (fn [e]
      (= value (get-in e [:attrs attr])))))

(defmethod element-filter :expr [m]
  (let [form (:form m)]
    (fn [e]
      (form (meta e)))))

(defn element-matcher [[tag filters]]
  (let [tag-matcher #(= tag (:tag %))
        filters (reduce #(and %1 %2) )]))

(defn build-match-expression [expr]
  (loop [[cond & more] (match-expr-parser expr)
         result []]
    (if (nil? cond)
      result
      (recur more (conj result (element-matcher cond))))))
