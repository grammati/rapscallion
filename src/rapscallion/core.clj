(ns
  ^{:author "Chris Perkins"
    :doc "The Rapscallion XML generation and transformation library."}
  rapscallion.core
  (:import (java.io Reader StringReader PushbackReader))
  (:require (clojure
             (string :as string)
             (pprint :as pprint)
             (walk :as walk)))
  (:require (rapscallion
             (xml :as xml)))
  (:require (imparsonate
             (core :as imp)
             (lib :as implib)))
  )


(defn- compile-error [& messages]
  (throw (IllegalArgumentException. (apply str messages))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extract embedded expressions from text

(defn slurp-reader
  "Consume the given Reader and return its contents as a string."
  [#^Reader reader]
  (let [sb (StringBuilder.)]
    (loop [c (.read reader)]
      (if (neg? c)
        (.toString sb)
        (do
          (.append sb (char c))
          (recur (.read reader)))))))

(defn read-partial
  "Like (read s), but returns a 2-element vector of the form
   that was read and the remaining, unread portion of the string."
  [s]
  (let [rdr (PushbackReader. (StringReader. s))]
    [(read rdr) (slurp-reader rdr)]))

(defn read-all [s]
  (loop [forms [] s s]
    (if (empty? s)
      forms
      (let [[form s] (read-partial s)]
        (recur (conj forms form) (.trim s))))))

(defn read-one [s]
  (let [[form & more] (read-all s)]
    (when more
      (compile-error "Expected exactly one clojure form - saw \"" s "\""))
    form))

(defn- merge-adjacent-strings
  [col]
  (let [joiner #(if (string? (first %)) [(apply str %)] %)
        joined (map joiner (partition-by string? col))]
    (apply concat joined)))


(defn extract-exprs
  "Extract embedded expressions from the string, returning a sequence of strings and forms."
  ([s] (extract-exprs s "$"))
  ([s c]
    (let [pat (java.util.regex.Pattern/quote c)]
      (loop [parts [] s s]
        (let [[left rest] (seq (.split s pat 2)) ; FIXME - $ as last character gets ignored
              parts (if (not-empty left) (conj parts left) parts)]
          (cond
            (empty? rest) ; at the end
              (merge-adjacent-strings parts)
            (.startsWith rest c) ; doubled $$ == literal $
              (recur (conj parts c) (.substring rest 1))
            :else
              (let [brace? (.startsWith rest "{")
                    rest (if brace? (.substring rest 1) rest) 
                    [form right] (read-partial rest)]
                (when (and brace? (not (.startsWith right "}")))
                  (throw (Exception. "mismatched curly-bracket in embedded code"))) ; FIXME - exception class, line number, etc.
                (recur (conj parts form) (if brace? (.substring right 1) right)))))))))
                


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XML template language

; directives:
;name      element   attr     notes
;----------------------------------------------------------------------
;args      no        yes      top-level element only?
;if        yes       yes      <rap:if test="...">
;for       yes       yes      <rap:for expr="...">
;let       yes       yes      <rap:let bindings="...">
;attrs     no        yes      
;meta      no        yes      
;defn      yes       yes      <rap:defn fn="name [args]">
;call      yes       yes      <rap:call fn="name args">
;match     yes       yes      <rap:match xpath="foo/bar{:cool}">


(def *strict* true) ; for debugging - don't change this to false unless you know what you're doing

(def *keep-whitespace* false)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Each value that is substituted into a template gets passed through
;; this multimethod. Eg:
;;   <foo>Hello ${name}</foo>
;; compiles to
;;   (Element. :foo nil ["Hello " (xml-value name)])

(defmulti xml-value type)

;; The default is just to stringify the object.
(defmethod xml-value :default [x] 
  (str x))

;; Elements are inserted as-is.
(defmethod xml-value xml/element-type [x]
  x)

(defmethod xml-value clojure.lang.Sequential [x]
  (map xml-value x))

;; This is a special case that handles "call-with-content".
;; Example:
;;   <rap:call fn='foo'>
;;     <bar>Hi</bar>
;;   </rap:call>
;; This generates code that will call "foo" with one parameter - a fn
;; that returns (Element. :bar nil ["Hi"]). That fn is flagged in its
;; metadata as a :call-body. Inside the definition of "foo", it can
;; be inserted as either $x or $(x) (assuming it is bound to "x"
;; inside foo). Why? Consider the definition of foo:
;;   <rap:defn fn='foo [x]'>
;;     <wrap>$x</wrap>
;;   <rap:defn>
;; The author of this function cannot know whether to use $x or $(x),
;; because he doesn't know how it will be called:
;;   $(foo 23)
;;   <rap:call fn='foo'>...etc...</rap:call>
;; 
(defmethod xml-value ::call-body [f]
  (f))



(defmulti compile-xml type)

(defmethod compile-xml String [s]
  (let [s (if *keep-whitespace* s (string/trim s))
        parts (extract-exprs s)]
    (if (and (= 1 (count parts)) (string? (first parts)))
      (first parts)
      (vec (for [p parts] (if (string? p) p `(xml-value ~p)))))))

(defn flatseq [& cols]
  (filter (complement nil?) (flatten cols)))
  

(defn process-attr [v]
  (let [v (extract-exprs v)]
    (if (> (count v) 1)
      `(str ~@v)
      (first v))))

(defn process-attrs [elt]
  (assoc elt :attrs (into {} (for [[k v] (:attrs elt)] [k (process-attr v)]))))

(defn directive? [elt]
  (.startsWith (name (:tag elt)) "rap:")) ; FIXME - use xmlns

(defn- strip-ns [qname]
  (-> qname name (.split ":" 2) second keyword))


(defmulti compile-directive 
  (fn 
    ([elt]
      (when-not (directive? elt) 
        (compile-error "compile-directive called, but element is not a directive. tag: " (:tag elt)))
      (strip-ns (:tag elt)))
    ([tag & _] tag)))

(defmethod compile-directive :default [elt]
  (compile-error "Unknown directive: " (:tag elt)))


(defn pop-directive-attrs
  "Given a directive-element and an attribute name, returns a two-element
   vector of the element with the attribute removed, and the attribute parsed
   into clojure forms."
  [elt attr]
  (let [tag (:tag elt)
        [elt expr] (xml/pop-attr elt attr)]
    (when (empty? expr)
      (compile-error "Attribute " attr " is required on directive " tag))
    (when (not-empty (:attrs elt))
      (compile-error "Unsupported attibutes on " tag " directive: " (string/join " " (keys (:attrs elt)))))
    [elt (read-all expr)]))

(defmethod compile-directive :let
  ([elt]
    (let [[elt bindings] (pop-directive-attrs elt :bindings)]
      (compile-directive :let bindings (:content elt))))
  ([_ bindings body]
    `(let [~@bindings] ~body)))
     
    
(defmethod compile-directive :if 
  ([elt]
    (let [[elt test] (pop-directive-attrs elt :test)]
      (compile-directive :if test (:content elt))))
  ([_ [test & more :as exprs] body]
    (when more
      (compile-error "Only one form is allowed as the 'test' attribute of an :if directive. Found: " exprs))
    `(if ~test ~body)))

(defmethod compile-directive :for 
  ([elt]
    (let [[elt bindings] (pop-directive-attrs elt :bindings)]
      (compile-directive :for bindings (:content elt))))
  ([_ bindings body]
    `(for [~@bindings] ~body)))
    
(defmethod compile-directive :attrs
  ([elt]
     (compile-error "Directive :attrs is not allowed as an element."))
  ([_ [expr & more :as exprs] elt]
     (when more
       (compile-error ":attrs directive must be a single form - saw " exprs))
     `(xml/merge-attrs ~elt ~expr)))

(defn merge-meta [obj m]
  (with-meta obj (into (or (meta obj) {}) (if (keyword? m) {m true} m))))

(defmethod compile-directive :meta
  ([elt]
     (compile-error "Directive :meta is not allowed as an element."))
  ([_ [expr & more :as exprs] elt]
     (when more
       (compile-error ":meta directive must be a single form - saw " exprs))
     `(merge-meta ~elt ~expr)))

(defmethod compile-directive :call
  ([elt]
     (let [[elt body-args] (xml/pop-attr elt :args)
           body-args (and body-args (read-all body-args))
           [elt fn-and-args] (pop-directive-attrs elt :fn)]
       (compile-directive :call fn-and-args (:content elt) body-args)))
  ([elt fn-and-args body]
     (compile-directive elt fn-and-args body nil))
  ([_ [f & args] body body-args]
     `(~f ~@args (with-meta (fn [~@body-args] ~body) {:type ::call-body}))))
    
    
(defn compile-element [elt]
  (if (directive? elt)
    (compile-directive elt)
    elt))


(defn extract-directives 
  "Pull rap: directives out of the :attrs map and put them in metadata instead."
  [elt]
  (let [is-directive (fn [[k v]] (.startsWith (name k) "rap:")) ; FIXME - use xmlns
        attrs        (:attrs elt)
        directives   (filter is-directive attrs)
        directives   (into {} (for [[k v] directives] [(strip-ns k) (read-all v)]))
        attrs        (into {} (filter (complement is-directive) attrs))
        elt          (assoc elt :attrs attrs)]
    (vary-meta elt assoc :directives directives)))
  
(defn pop-directive [elt name]
  (let [directives (:directives (meta elt))
        value      (get directives name)
        directives (dissoc directives name)
        elt        (vary-meta elt assoc :directives directives)]
    [elt value]))


(def *directives* (atom [{:name :attrs}
                         {:name :meta}
                         {:name :call}
                         {:name :let}
                         {:name :if}
                         {:name :for}
                         ]))


(defn apply-directives
  "Given an element, return the element with any directives attached as attributes compiled."
  ([elt] (apply-directives elt @*directives*))
  ([elt directives]
     (let [directive-values (:directives (meta elt))
           elt (vary-meta elt dissoc :directives)
           body (compile-element elt)]
       (loop [directive-values directive-values
              body body
              [next-directive & directives] directives]
         (if (empty? directive-values)
           body ;; done - all directives attached to this element have been processed
           (if-not next-directive
             (when *strict*
               (compile-error "Unprocessed directives on element " (:tag elt)
                            " (" (string/join ", " (keys directive-values)) ")."))
             (let [directive-name (:name next-directive)
                   directive-value (get directive-values directive-name)]
               (recur (dissoc directive-values directive-name)
                      (if directive-value
                        (compile-directive directive-name directive-value body)
                        body)
                      directives))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rap:defn

(defn- defn? [elt]
  (and (xml/element? elt)
    (or 
      (= (:tag elt) :rap:defn) 
      (get-in elt [:attrs :rap:defn]))))

(defn- make-defn [elt]
  (if (= (:tag elt) :rap:defn)
    (let [[elt fnspec] (pop-directive-attrs elt :fn)]
      `(~@fnspec (flatseq ~@(map compile-xml (:content elt)))))
    (let [[elt fnspec] (xml/pop-attr elt :rap:defn)
          [name argvec] (read-all fnspec)]
      `(~name [~@argvec] ~(compile-xml elt)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rap:match

(imp/defparser match-expr-parser
  :root           :path
  :path           #{:relpath :abspath}
  :abspath        ["/" :relpath]
  :relpath        (imp/list-of :elt "/")
  
  :elt            [#{:tag "*"} :filter*]
;                    (fn [tag filters] {:tag tag :filters filters})
  
  :tag            #"[\w-]+"
                    keyword
  
  :filter         #{:xml-filter :clj-filter}
  :attr-filter    ["@" :attrname ["=" :quoted-string]:?]
                    (fn [n v] {:type :attr :name (keyword n) :value v})
                    
  :attrname       #"[\w-]+" ;FIXME
  :quoted-string  implib/double-quoted-string
  
  
  :xml-filter     ["[" :attr-filter "]"]
  
  :clj-filter     ["{" :clj-form "}"]
                    (fn [form] {:type :expr :form form})
                    
  :clj-form       read-partial
  
  )



(defn compile-element-filter [[tag filters]]
  (let [elt-sym (gensym "elt")]
    `(fn [~elt-sym]
       (and
        (= ~tag (:tag ~elt-sym))
        ~@(for [{type :type :as f} filters]
            (case type
                  :attr (if-let [v (:value f)]
                          `(= ~v (get-in ~elt-sym [:attrs ~(:name f)]))
                          `(not (nil? (get-in ~elt-sym [:attrs ~(:name f)]))))
                  :expr `(~(:form f) (meta ~elt-sym))))))))

(defn compile-match-expression
  ""
  [expr]
  {:pre [(not (empty? expr))]}
  (map compile-element-filter (match-expr-parser expr)))

(defn matcher? [elt]
  (and (xml/element? elt)
    (or
      (= (:tag elt) :rap:match)
      (get-in elt [:attrs :rap:match]))))

(defn- map-content [elt f]
  (assoc elt :content (flatseq (map f (:content elt)))))

(defn set-content [elt & content]
  (assoc elt :content (flatseq content)))

(defn append-content [elt & new-content]
  (apply set-content elt (:content elt) new-content))

(defn match-filter
  "Build code for a function that will replace elements matching the
   given expression with the return value of replacer."
  [[match-fn & more-fns] replacer recursive?]
  (fn m [elt]
    (if (xml/element? elt)
      (if (match-fn elt)
        (if more-fns
          (map-content elt (match-filter more-fns replacer false))
          (replacer elt))
        (if recursive?
          (map-content elt m)
          elt))
      elt)))

(defn compile-matcher
  ([expr body action as-sym]
     (let [action     (keyword (or action :replace))
           as-sym     (or as-sym (gensym "matched-element"))
           as-sym     (if (string? as-sym) (read-one as-sym) as-sym)
           body       (into [] (map compile-xml body))
           expr       (string/trim expr)
           [expr recursive?] (if (.startsWith expr "/")
                               [(.substring expr 1) false]
                               [expr true])
           exprs      (compile-match-expression expr)
           fn-body    (case action
                            :replace body
                            :append  `(append-content ~as-sym ~@body)
                            (compile-error "Unknown value for 'action' attribute on rap:match elment: " action))]
       `(match-filter [~@exprs] (fn [~as-sym] ~fn-body) ~recursive?))))

(defn make-matcher [elt]
  (if (= (:tag elt) :rap:match)
    (let [[elt [expr action as-sym]] (xml/pop-attrs elt :expr :action :as)]
      (when-not expr
        (compile-error "Attribute :expr is required on rap:match element."))
      (when-not (empty? (:attrs elt))
        (compile-error "Unrecognized attributes on rap:match element:" (keys (:attrs elt))))
      (compile-matcher expr (:content elt) action as-sym))
    (let [[elt [expr action as-sym]] (xml/pop-attrs elt :rap:match :rap:action :rap:as)]
      (compile-matcher expr [elt] action as-sym))))
      

(defn compile-content
  ""
  [content]
  (let [[elt & more :as content] content]
    (cond
      (nil? elt)
        nil
      (defn? elt)
        (let [[defns others] (split-with defn? content)]
          `(letfn [~@(map make-defn defns)]
            ~(compile-content others)))
      (matcher? elt)
        `(let [matcher# ~(make-matcher elt)] 
          (map matcher# ~(compile-content more)))
      :else
        `[~(compile-xml elt) ~@(compile-content more)])))

(defn process-content [elt]
  (let [compiled (compile-content (:content elt))
        compiled (if (nil? compiled) nil `(flatseq ~compiled))] ;TODO - be smarter here
    (assoc elt :content compiled)))
        
(defmethod compile-xml xml/element-type [elt]
  (-> elt
      extract-directives
      process-content
      process-attrs
      apply-directives
      ))

(defn- de-elementize
  "We work with Element instances at compile-time, but they do not
   self-evaluate like normal maps do. Convert them into an eval-able form."
  [x]
  (walk/prewalk
   (fn [e]
     (if (instance? xml/element-type e)
       (list 'rapscallion.xml.Element. (:tag e) (:attrs e) (:content e))
       e))
   x))

(defn to-template-fn
  "Given a template, returns a data structure that can be eval-ed into a function."
  [xml-in]
  (let [root     (xml/parse xml-in)
        [root [args requires uses imports]]
                 (xml/pop-attrs root :rap:args :rap:require :rap:use :rap:import)
        args     (read-all args)
        root     (with-meta root {::root true})
        compiled (-> root compile-xml de-elementize)
        ]
    `(do
       ~@(for [lib (read-all requires)] `(require '~lib))
       ~@(for [lib (read-all uses)]     `(use '~lib))
       ~@(for [lib (read-all imports)]  `(import '~lib))
       (fn [{:keys [~@args]}] ~compiled))))

(defn dump
  "Print the code generated for the given XML template."
  [template]
  (pprint/pprint (to-template-fn template)))

(declare *template-ns*)

(defn- eval-in-ns
  "Eval the "
  ([form]
     (eval-in-ns form *template-ns*))
  ([form ns-or-name]
     (binding [*ns* (if (symbol? ns-or-name) (create-ns ns-or-name) ns-or-name)]
       (eval form))))

; Namespace in which to eval templates
(defonce *template-ns* 
  (let [ns-name (gensym "template-ns")]
    (eval-in-ns
     '(clojure.core/with-loading-context
        (clojure.core/refer 'clojure.core))
     ns-name)
    (find-ns ns-name)))

(defn compile-template
  "Compiles the template into a function that takes a context-map as input
  and produces a lazy sequence of xml-event objects."
  [xml-in]
  (-> xml-in
      to-template-fn
      eval-in-ns
      (with-meta {::template-fn true ::template-source xml-in})
      ))

(defn template? [template]
  (fn? template)) ;TODO - attach metadata

(defmacro locals []
  (into {} (for [[k _] &env] [(keyword k) k])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API

(declare *template-loader*)

(defn template-loader
  "Returns a function that will return template, given a string (path or XML)."
  [root]
  (let [cache (atom nil)]
    ^{:root root}
    (fn loader [source]
      (if (and (string? source) (.startsWith source "<"))
        (compile-template source) ;don't cache strings
        (let [path  (string/join "/" [root source])
              f     (java.io.File. path)
              mod-t (.lastModified f)
              [cached-template cached-mod-t] (get @cache path)]
          (if (= cached-mod-t mod-t)
            cached-template
            (let [compiled (binding [*template-loader* loader]
                             (compile-template f))]
              (swap! cache assoc path [compiled mod-t])
              compiled)))))))

(def *template-loader* (template-loader nil))

(defn template
  "Returns a compiled template, either by compiling it or by getting it
   from the template cache."
  [source]
  (*template-loader* source))


(defn render
  "Render a template as a string of XML"
  ([template]
     (render template {}))
  ([template context]
     (let [tmpl (if (template? template) template (compile-template template))
           root-elt (tmpl context)]
       (xml/emit root-elt))))

(defmacro render-with-locals [template]
  `(render ~template (locals)))

