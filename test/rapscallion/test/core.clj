(ns rapscallion.test.core
  (:require (rapscallion 
             (core :as rap)
             (xml :as xml))
            (clojure
             (string :as string)))
  (:use [clojure.test])
  )

(defn xml-str= [a b]
  (letfn [(squish [x]
            (-> x
                string/trim
                (string/replace #">\s*<" "><")))]
    (= (squish a) (squish b))))

(defn xml= [a b]
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





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; playground

(defn bench-fn [f]
  (let [time-f (fn [n]
                 (let [start (System/nanoTime)]
                   (dotimes [_ n]
                     (f))
                   (- (System/nanoTime) start)))
        devnul (proxy [java.io.Writer] []
                 (write [])
                 (flush []))]
                                        ;(binding [*out* devnul])
    (loop [n 1]
      (let [t (time-f n)]
        (if (< t 1e8)
          (recur (* 2 n))
          (double ( / t n)))))))

(defmacro bench [& body]
  `(let [ns# (bench-fn (fn [] ~@body))
         [n# u#] (cond
                  (> ns# 1e9) [(/ ns# 1e9) "s"]
                  (> ns# 1e6) [(/ ns# 1e6) "ms"]
                  (> ns# 1e3) [(/ ns# 1e3) "us"]
                  :else       [ns# "ns"])]
     (println (format "%g %s" n# u#))))
