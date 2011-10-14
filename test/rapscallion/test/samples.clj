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

(defn test-sample-file [test-file compile-fn compare-fn]
  (println "Testing: " test-file)
  (let [[template & testcases] (map #(.trim %) (string/split (slurp test-file) #"={8,}"))
        template (compile-fn template)]
    (when (< (count testcases) 2)
      (throw (Exception. (str "Missing testcase in sample file " test-file))))
    (doseq [[input expected] (partition 2 testcases)]
      (let [input (eval (read-string input))]
        (is (compare-fn expected (template input)))))))

(defmacro test-samples [dirname ext compile-fn compare-fn]
  (let [tests (for [test-file (sample-files dirname ext)]
                `(deftest ~(symbol (string/replace test-file #"[^\w-]" "-"))
                   (test-sample-file ~test-file ~compile-fn ~compare-fn)))]
    `(do ~@tests)))

(test-samples "./test/resources/samples"
              ".xml"
              rap/template
              (fn [expected result]
                (xml= expected (xml/emit result))))

(test-samples "./test/resources/samples"
              ".txt"
              text/template
              (fn [expected result]
                (= (.trim expected) (.trim result))))


(defn test-suite
  "Given a directory, loads a template called template.xml, then
  renders it for each input loaded from a file called input-N.clj. The
  result is compared to the contents of expected-N.xml, for each N."
  [dir]
  (println "Testing: " dir)
  (let [template (rap/template (str dir "/template.xml"))
        inputs   (->> dir
                      directory-listing
                      (map #(re-find #"expected-(\d+).clj$" %))
                      (filter identity))]
    (when (empty? inputs)
      (throw (Exception. (str "No inputs found for test suite: " dir))))
    (doseq [[input-file n] inputs]
      (let [input (->> input-file
                       (str dir "/")
                       slurp
                       read-string
                       eval)
            output (rap/render template input)
            expected (slurp (str dir "/expected-" n ".xml"))]
        (is (xml= output expected))))))

(defmacro test-suites [dirname]
  (let [suites (for [subdir (directory-listing dirname)]
                `(deftest ~(symbol (string/replace subdir #"[^\w-]" "-"))
                   (test-suite ~(string/replace subdir #"\\" "/"))))]
    `(do ~@suites)))

(test-suites "./test/resources/suites")


