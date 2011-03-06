(ns rapscallion.test.core
  (:require (rapscallion 
              (core :as rap)
              (xml :as xml)))
  (:use [clojure.test])
  )

(defn xml= [a b]
  (= (xml/parse a) (xml/parse b)))

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




