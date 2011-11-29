(ns rapscallion.test.xml
  (:require [rapscallion 
             [xml :as xml]]
            [clojure
             [string :as string]])
  (:use [clojure.test]))


(defn xml-str=
  "Compare two XML strings, ignoring whitespace."
  [a b]
  (letfn [(squish [x]
            (-> x
                string/trim
                (string/replace #">\s+<" "><")))]
    (= (squish a) (squish b))))

(defn xml=
  "Compare two XML objects."
  [a b]
  (try
    (= (xml/parse a) (xml/parse b))
    (catch Exception x
      (xml-str= a b))))


(def xml-1 "<a><b/></a>")
(def xml-2 "<a a1='v1'><b a2=\"v2\"></a>")

(deftest parse-no-namespaces
  (let [xml (str "<a xmlns='ns-1'>"
                 " <b")]))