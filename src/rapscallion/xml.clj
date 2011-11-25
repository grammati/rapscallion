(ns rapscallion.xml
  (:require (clojure [xml :as xml]
                     [string :as  string])
            (clojure.java [io :as io])
            (yoodls [pipe :as pipe]))
  (:import [java.io Writer Reader StringReader]
           [java.util.concurrent LinkedBlockingQueue]
           [org.xml.sax Attributes InputSource]
           [org.xml.sax.ext DefaultHandler2]
           [org.xml.sax.helpers XMLReaderFactory]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocols
(defprotocol ElementAccessor
  (tag [this] "Return the tag of the element")
  (attrs [this] "Return a map of the attributes of the element")
  (content [this] "Return a sequence of children of the element"))

(defprotocol XMLWritable
  (emit-xml [this ^Writer out]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Records for XML node types

(declare emit-element emit-comment emit-cdata emit-pi)

(defrecord Element [tag attrs content]
  
  ElementAccessor
  (tag [this] tag)
  (attrs [this] attrs)
  (content [this] content)
  
  XMLWritable
  (emit-xml [this out]
    (emit-element this out))
  )

(defrecord Comment [^String text]
  XMLWritable
  (emit-xml [this out]
    (emit-comment this out)))

(defrecord CData [^String text]
  XMLWritable
  (emit-xml [this out]
    (emit-cdata this out)))

(defrecord PI [^String target ^String text]
  XMLWritable
  (emit-xml [this out]
    (emit-pi this out)))


(defn element
  "Factory function to return an element."
  ([tag] (element tag nil nil))
  ([tag attrs] (element tag attrs nil))
  ([tag attrs content] (Element. tag attrs content)))

(defn element?
  "Returns true if the given object is an element."
  [e]
  (instance? Element e))

;;; Alias for the Element class
(def element-type Element)

(defn- elementize
  "This is used to turn a tree of element-like maps into a tree of
  Element instances."
  ;; TODO - is this still used anywhere?
  [e]
  (if (and (not (element? e))
           (map? e)
           (:tag e))
    (Element.
     (:tag e)
     (:attrs e)
     (into [] (map elementize (:content e))))
    e))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

(defn get-attr [elt attr]
  (get-in elt [:attrs attr]))

(defn assoc-attr [elt & attr-vals]
  (assoc elt :attrs (apply assoc (:attrs elt) attr-vals)))

(defn merge-attrs [elt new-attrs]
  (assoc elt :attrs (merge (:attrs elt) new-attrs))) ;TODO - skip nils

(defn dissoc-attr [elt & attrs]
  (assoc elt :attrs (apply dissoc (:attrs elt) attrs)))

(defn pop-attr [elt attr]
  [(dissoc-attr elt attr) (get-attr elt attr)])

(defn pop-attrs [elt & attrs]
  (loop [elt elt
         [attr & attrs] attrs
         vals []]
    (if attr
      (let [[elt val] (pop-attr elt attr)]
        (recur elt attrs (conj vals val)))
      [elt vals])))

(defn append-content [elt & children]
  (update-in elt [:content] concat children))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing XML

(defn parse-error [& messages]
  (throw (RuntimeException. (apply str messages))))

;; swiped from data.xml:
(defn attrs->map[^Attributes atts]
  (into {} (for [^Long i (range (.getLength atts))]
             [(keyword (.getQName atts i))
              (.getValue atts i)])))

(defn xml-handler
  "Return an instance of org.xml.sax.ext.DefaultHandler2 that will
  handle SAX events by calling the supplied function, passing a vector
  of 1, 2 or 3 items, the first being the event type as a keyword, and
  the other 2 (optional) being the associated data.

  The event types and their associated data are:
    [:start-element tag attrs]
    [:end-element tag]
    [:text string]
    [:start-cdata]
    [:end-cdata]
    [:processing-instruction target data]
    [:start-prefix-mapping prefix uri]
    [:end-prefix-mapping prefix]
    [:comment text]
  "

  [f]
  (proxy [DefaultHandler2] []
    
    ;; Elements
    (startElement [uri local-name q-name ^Attributes atts]
      (f [:start-element (keyword q-name) (attrs->map atts)]))
    (endElement [uri local-name q-name]
      (f [:end-element (keyword q-name)]))

    ;; Text
    (characters [^chars ch ^long start ^long length]
      (f [:text (String. ch start length)]))

    ;; Keep track of which text came from a CDATA section
    (startCDATA []
      (f [:start-cdata]))
    (endCDATA []
      (f [:end-cdata]))

    ;; Processing Instructions
    (processingInstruction [target data]
      (f [:processing-instruction target data]))

    ;; Mapping prefixes to namespace URIs
    (startPrefixMapping [prefix uri]
      (f [:start-prefix-mapping prefix uri]))
    (endPrefixMapping [prefix]
      (f [:end-prefix-mapping prefix]))

    ;; Comments
    (comment [^chars ch ^long start ^long length]
      (f [:comment (String. ch start length)]))
    
    ))


(defprotocol AsInputSource
  (as-input-source [this] "Wrap this object as an instance of org.xml.sax.InputSource"))

(extend-protocol AsInputSource
  String
  (as-input-source [s]
    (let [^Reader r (if (.startsWith s "<") ;; too "clever"? ... <shrug>
                      (StringReader. s)
                      (io/reader s))]
      (InputSource. r)))

  InputSource
  (as-input-source [this] this)
  
  Object
  (as-input-source [o]
    (InputSource. (io/reader o))))


(defn sax-seq
  "Given a source of XML, returns a sequence of events, in the form of
  vectors of [type data ...]. See xml-handler for details."
  [in & [opts]]
  (let [{:keys [namespaces]
         :or   {namespaces true}} opts
         
        ^InputSource in (as-input-source in)
        parser          (XMLReaderFactory/createXMLReader)
        [put event-seq] (pipe/pipe)
        handler         (xml-handler put)]
    
    (.setContentHandler parser handler)
    
    ;; Set the "lexical-handler" property in order to get
    ;; start-prefix-mapping & end-prefix-mapping events.
    (.setProperty parser "http://xml.org/sax/properties/lexical-handler" handler)
    
    ;; Setting this to true means that undeclared namespace-prefixes
    ;; will be an error.
    (.setFeature parser "http://xml.org/sax/features/namespaces" namespaces)

    ;; Run the SAX parser in a thread.
    (future
      (try
        (.parse parser in)
        (catch Exception e
          (put [:error e]))
        (finally
         (put nil))))

    event-seq))

;;; Transformations of event streams
(defn throw-if-error [[type ex :as evt]]
  (if (= :error type)
    (throw ex)
    evt))

(defn throw-on-error [events]
  (map throw-if-error events))


(defn merge-adjacent-text
  "Transform the event stream so that adjacent bits of text are
   emitted as a single text event."
  [events]
  (letfn [(text? [[type _]]
            (= :text type))
          (merge-if-text [[e & _ :as  evts]]
            (if (text? e)
              (list [:text (apply str (map second evts))])
              evts))]
    (->> events
         (partition-by text?)
         (map merge-if-text)
         (apply concat))))


(defn skip-whitespace [events]
  (remove (fn [[type s :as evt]]
            (and (= type :text)
                 (string/blank? s)))
          events))

(defn ignore
  "Filters out events of the given types."
  [events & event-types]
  (let [ignored-types (set event-types)
        ignorable? (fn [[type]] (ignored-types type))]
    (remove ignorable? events)))


(defn events->nodes
  "Takes a seq of SAX events, and returns a 2-element vector of:
   1 - A seq of nodes extracted from the event seq, and
   2 - A seq of the unconsumed events.
  "
  [events]
  (loop [nodes []
         [[type data :as evt] & events] events
         ns-stack (list nil)]
    
    (case type
      
      :start-prefix-mapping
      (let [[_ prefix uri] evt
            ns-map (assoc (peek ns-stack) uri prefix)]
        (recur nodes events (conj ns-stack ns-map)))

      :end-prefix-mapping
      (recur nodes events (pop ns-stack))
      
      :start-element
      (let [[_ tag attrs]     evt
            [children events] (events->nodes events)
            elt               (Element. tag attrs children)]
        (recur (conj nodes (with-meta elt {:xmlns (peek ns-stack)}))
               events
               ns-stack))

      :text
      (recur (conj nodes data)
             events
             ns-stack)

      :comment
      (recur (conj nodes (Comment. data))
             events
             ns-stack)

      :start-cdata
      (let [[text-nodes [end-cdata & events]]
            (split-with (fn [[type]] (= type :text)) events)]
        (assert (= (end-cdata 0) :end-cdata) "Expected :end-cdata event.")
        (recur (conj nodes (CData. (apply str (map second text-nodes))))
               events
               ns-stack))

      :processing-instruction
      (let [[_ target text] evt]
        (recur (conj nodes (PI. target text))
               events
               ns-stack))

      (:end-element nil)
      [nodes events]
      
      )))

(defn make-tree
  "Takes a seq of SAX events and returns a vector of the top-level nodes."
  [events]
  (let [[nodes events] (events->nodes events)]
    (when (seq events)
      ;; This shouldn't be possible if the source of the events is an
      ;; actual SAX parser, but could happen for an event-seq from
      ;; some other source.
      (parse-error "Unconsumed SAX events: " events))
    nodes))

;; New default parse options.
(def default-parse-options
  {:namespaces      true
   :keep-whitespace false
   :comments        true
   :cdata           true
   :processing-instructions true
   })

;; For backward-compatibility. Options for parsing clojure.xml-style.
(def old-parse-options
  {:namespaces      false
   :keep-whitespace false
   :comments        false
   :cdata           false
   :processing-instructions false
   })

(def ^{:dynamic true} *parse-options* default-parse-options)

(defn parse
  "Takes something convertable to a InputSource, and returns a vector
   of top-level XML nodes.

   The opts parameter is an optional map of options. The defaults for
   all options are in the map *parse-options*, which is rebindable.

     :keep-whitespace
       If true, whitespace-only text nodes are kept, othewise they
       are discarded.

     :comments
     :cdata
     :processing-instructions
       If false, nodes of the corresponding type are discarded.

     :namespaces
       Sets namespace-awareness in the SAX parser.
   "
  
  [source & [opts]]
  
  ;; This works by producing a seq of SAX-events, and then running it
  ;; through a series of transformations. This should allow quite a
  ;; bit flexibility in deciding exactly which transformations to
  ;; apply, at the cost of being slightly less efficient - for
  ;; example, events for comments, processing-instructions, etc. may
  ;; be produced by the parser and added to the seq, but then
  ;; ignored. This inefficiency should be ameliorated somewhat by the
  ;; fact that those types of nodes relatively rare in XML.
  
  (let [;; Use defaults for options unless overridden.
        {:keys [namespaces
                comments cdata processing-instructions
                keep-whitespace]} (merge *parse-options* opts)

        ;; First, run the SAX parser to produce the full sequence of
        ;; events. Give it the nice short name "e", since it will be
        ;; repeated a lot.
        e (sax-seq source opts)

        ;; Since the SAX parser runs in another thread, parsing errors
        ;; are caught and put into the event-seq as [:error
        ;; <exception>].  This transformation will re-throw them.
        e (throw-on-error e)

        ;; SAX parsers can emit multiple "characters" events for one
        ;; piece of text (for example, when it contains entities, such
        ;; as &amp;). It's generally more convenient to work with
        ;; adjacent text nodes merged into one.
        e (merge-adjacent-text e)

        ;; Optionally discard whitespace-only nodes
        e (if keep-whitespace e (skip-whitespace e))

        ;; Figure out which types of node to ignore.
        ignores (reduce into nil
                        [(when-not comments [:comment])
                         (when-not cdata [:start-cdata :end-cdata])
                         (when-not processing-instructions [:processing-instruction])
                         ;; Maybe don't need this line (the parser
                         ;; won't emit these anyway, right?):
                         (when-not namespaces [:start-prefix-mapping :end-prefix-mapping])
                         ])

        ;; Skip ignorable events
        e (if ignores (apply ignore e ignores) e)
        ]

    ;; Finally, turn the resulting event-seq into a tree (almost) of
    ;; nodes. This actually returns all the top-level nodes, which
    ;; should consist of exactly one Element, as well as any number of
    ;; Comments and PIs preceeding and/or following the Element.
    (make-tree e)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emitting XML

(defn ^String xml-escape [^String s]
  (-> s
      (.replaceAll "&"  "&amp;")
      (.replaceAll "<"  "&lt;")))

(defn ^String xml-escape-attr [^String s]
  (-> s
      xml-escape
      (.replaceAll "'"  "&apos;")))

(defn ^String xml-escape-comment [^String s]
  ;; Comment text can't contain -->
  (-> s
      xml-escape
      (.replaceAll "-->" "--&gt;")))


(defn emit-element [^Element e ^Writer out]
  (let [{:keys [tag attrs content]} e]
    (doto out
      (.write "<")
      (.write (name tag)))
    (doseq [[k v] attrs]
      (if-not (nil? v)
        (doto out
          (.write " ")
          (.write (name k))
          (.write "='")
          (.write (xml-escape-attr (str v)))
          (.write "'"))))
    (if (empty? content)
      (.write out "/>")
      (do
        (.write out ">")
        (doseq [child content]
          (emit-xml child out))
        (doto out
          (.write "</")
          (.write (name tag))
          (.write ">"))))))

(defn emit-comment [^Comment c ^Writer out]
  (doto out
    (.write "<!--")
    (.write (xml-escape-comment (.text c)))
    (.write "-->")))

(defn emit-cdata [^CData cd ^Writer out]
  (when (.contains (.text cd) "]]>")
    (throw (IllegalArgumentException. "Illegal end sequence in CDATA text.")))
  (doto out
    (.write "<![CDATA[")
    (.write (.text cd))
    (.write "]]>")))

(defn emit-pi [^PI pi ^Writer out]
  (let [{:keys [target text]} pi]
    ;; TODO - validate target: http://www.w3.org/TR/REC-xml/#sec-pi
    (when (.contains text "?>")
      (throw (IllegalArgumentException. "Illegal end sequence in processing instruction " target)))
    (doto out
      (.write "<?")
      (.write target)
      (.write " ")
      (.write text)
      (.write "?>"))))


(extend-protocol XMLWritable

  nil
  (emit-xml [_ _])
  
  clojure.lang.Sequential
  (emit-xml [s ^Writer out]
    (doseq [o s]
      (emit-xml o out)))
  
  String
  (emit-xml [s ^Writer out]
    (.write out (xml-escape s)))

  Object
  (emit-xml [o ^Writer out]
    (emit-xml (str o) out))
  
  )


(defn emit
  "Turn an element into a String of XML."
  [elt]
  (with-out-str
    (emit-xml elt *out*)))

