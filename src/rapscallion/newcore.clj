(ns
    ^{:author "Chris Perkins"
      :doc "The Rapscallion XML generation and transformation library."}
  rapscallion.newcore)



(def
  ^{:doc "Form that will be inserted into top of each template before evaling it."}
  *template-boilerplate*
  `(use '[rapscallion.xml :only [element]]))

(defn compile-template
  ""
  [source])


(declare ^:dynamic *template-loader*)

(defn template-loader
  "Returns a template loader that knows how to load templates from
  files under the given root directory."
  [root]
  (let [cache (atom nil)]
    ^{:root root}
    (fn loader [source]
      (if (and (string? source) (.startsWith source "<"))
        (compile-template source) ;don't cache strings
        (let [path  (string/join "/" [(or root \.) source])
              f     (java.io.File. path)
              mod-t (.lastModified f)
              [cached-template cached-mod-t] (get @cache path)]
          (if (= cached-mod-t mod-t)
            cached-template
            (let [compiled (binding [*template-loader* loader]
                             (compile-template f))]
              (swap! cache assoc path [compiled mod-t])
              compiled)))))))

(def ^:dynamic *template-loader* (template-loader nil))


(defn template
  "Returns a compiled template loaded from the given source.
   source can be a filename or a String of XML.  Optional loader
   argument specifies the template loader instance to use. If not
   supplied, used *template-loader* instead."
  ([source]
     (template source *template-loader*))
  ([source loader]
     (loader source)))


(defn template? [template]
  (::template-fn (meta template)))

(defmacro locals []
  (into {} (for [[k _] &env] [(keyword k) k])))


(defn render
  "Render a template as a string of XML."
  ;; TODO - should emit to a Writer rather than building a string
  ;; (more general).
  ([template]
     (render template {}))
  ([template context]
     (render template context *out*))
  ([template content out]
     (let [tmpl (if (template? template)
                  template
                  (compile-template template))
         elts (tmpl context)]
     (xml/emit elts))))

(defmacro render-with-locals
  ([template]
     (render-with-locals *out*))
  ([template out]
     `(render ~template (locals) ~out)))