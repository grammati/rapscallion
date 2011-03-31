(ns rapscallion.test.samples
  (:require (rapscallion 
              (core :as rap)
              (xml :as xml)
              (text :as text)
              ))
  (:require (clojure (string :as string)))
  (:use clojure.test)
  (:use (rapscallion.test (core :only [xml=])))
  )
  
(defn eval-file [#^java.io.File f]
  (eval (read (java.io.PushbackReader. (java.io.FileReader. f)))))

(def *samples-dir* "./test/resources/samples")

(defn directory-listing [dirname]
  (for [filename (.list (java.io.File. dirname))]
    (str dirname "/" filename)))

(deftest test-xml-samples
  (doseq [test-file (directory-listing *samples-dir*)]
    (when (and (.endsWith test-file ".xml") (not (.contains  test-file "#")))
      (println "Testing: " test-file)
      (let [[template & testcases] (map #(.trim %) (string/split (slurp test-file) #"={8,}"))
            template (rap/compile-template template)]
        (when (< (count testcases) 2)
          (throw (Exception. (str "Missing testcase in sample file " test-file))))
        (doseq [[input expected] (partition 2 testcases)]
          (let [input (eval (read-string input))]
            (is (xml= expected (xml/emit (template input))))))))))

(deftest test-text-samples
  (doseq [test-file (directory-listing *samples-dir*)]
    (when (and (.endsWith test-file ".txt") (not (.contains  test-file "#")))
      (println "Testing: " test-file)
      (let [[template & testcases] (map #(.trim %) (string/split (slurp test-file) #"={8,}"))
            template (text/compile-template template)]
        (when (< (count testcases) 2)
          (throw (Exception. (str "Missing testcase in sample file " test-file))))
        (doseq [[input expected] (partition 2 testcases)]
          (let [input (eval (read-string input))]
            (is (= (.trim expected) (.trim (template input))))))))))

