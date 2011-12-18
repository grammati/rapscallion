(ns rapscallion.test.core
  (:require [rapscallion 
             [core :as rap]]
            [eksemel
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


(deftest test-read-partial
  (is (= (rap/read-partial "nil") [nil ""]))
  (is (= (rap/read-partial "nil ") [nil " "]))
  (is (= (rap/read-partial "  nil ") [nil " "]))
  (is (= (rap/read-partial "(foo [bar] #(xyz 23)) junk") 
         ['(foo [bar] (fn* [] (xyz 23))) " junk"]))
  )
  
(deftest test-process-attrs
  (is (= (rap/process-attr "foo") "foo"))
  (is (= (rap/process-attr "$foo") 'foo))
  (is (= (rap/process-attr "foo$bar") `(str "foo" ~'bar)))
  )

(deftest test-render-with-locals
  (let [t "<foo rap:args='a b c'>$a $b $c</foo>"]
    (is (xml= "<foo>23  hello</foo>"
              (let [a 23 b nil c "hello"]
                (rap/render-with-locals t))))))




