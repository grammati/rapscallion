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

(defn directory-listing
  ([dirname]
     (directory-listing dirname (constantly true)))
  ([dirname filter]
     (for [filename (.list (java.io.File. dirname))
           :when (filter filename)]
       (str dirname "/" filename))))

(defn files-by-ext
  ([dirname ext]
     (files-by-ext dirname ext (constantly true)))
  ([dirname ext filter]
     (directory-listing dirname #(and (filter %) (.endsWith % ext)))))

(defn sample-files [dirname ext]
  (files-by-ext dirname ext #(and (not (.startsWith % "_")) (not (.contains % "#")))))


(defn test-samples [dirname ext compile-fn compare-fn]
  (doseq [test-file (sample-files dirname ext)]
    (println "Testing: " test-file)
    (let [[template & testcases] (map #(.trim %) (string/split (slurp test-file) #"={8,}"))
          template (compile-fn template)]
      (when (< (count testcases) 2)
        (throw (Exception. (str "Missing testcase in sample file " test-file))))
      (doseq [[input expected] (partition 2 testcases)]
        (let [input (eval (read-string input))]
          (is (compare-fn expected (template input))))))))

(deftest test-xml-samples
  (test-samples *samples-dir*
                ".xml"
                rap/compile-template
                (fn [expected result]
                  (xml= expected (xml/emit result)))))

(deftest test-text-samples
  (test-samples *samples-dir*
                ".txt"
                text/compile-template
                (fn [expected result]
                  (= (.trim expected) (.trim result)))))

