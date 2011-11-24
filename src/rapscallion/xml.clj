(ns rapscallion.xml
  (:require (clojure [xml :as xml])
            (clojure.java [io :as io]))
  (:use (yoodls [pipe :only [pipe]]))
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
;; Records

(declare emit-element)

(defrecord Element [tag attrs content]
  
  ElementAccessor
  (tag [this] tag)
  (attrs [this] attrs)
  (content [this] content)
  
  XMLWritable
  (emit-xml [this out]
    (emit-element this out))
  )


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
  "This is used to turn a tree of "
  [e]
  (if (and (map? e) (:tag e))
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

(defn dissoc-in [m ks]
  (let [k (last ks)
        ks (butlast ks)]
    (if ks
      (if-let [mm (get-in m ks)]
        (assoc-in m ks (dissoc mm k))
        m)
      (if k
        (dissoc m k)
        m))))

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
  (input-source [this] "Wrap this object as an instance of org.xml.sax.InputSource"))

(extend-protocol AsInputSource
  String
  (input-source [s]
    (let [^Reader r (if (.startsWith s "<") ;; too "clever"? ... <shrug>
                      (StringReader. s)
                      (io/reader s))]
      (InputSource. r)))
  
  Object
  (input-source [o]
    (InputSource. (io/reader o))))


(defn event-seq
  "Given a source of XML, returns a sequence of events, in the form of
  vectors. See xml-handler."
  [in]
  (let [^InputSource in (input-source in)
        parser  (XMLReaderFactory/createXMLReader)
        [put events] (pipe)
        handler (xml-handler put)]
    (.setContentHandler parser handler)
    (.setProperty parser "http://xml.org/sax/properties/lexical-handler" handler)
    (future
      (try
        (.parse parser in)
        (catch Exception e
          (put [:error e]))
        (finally
          (put nil))))
    events))

;;; Transformations of event streams
(defn merge-text
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


(defn ignore
  "Filters out events of the given types."
  [events & event-types]
  (let [ignored-types (set event-types)
        ignorable? (fn [[type]] (ignored-types type))]
    (remove ignorable? events)))


(defn to-nodes
  "Returns a 2-element vector containing:
   1) A seq of sibling nodes extracted from the event stream, and
   2) A seq of the unconsumed events.
  "
  [events]
  (println events)
  (loop [[[type :as evt] & events] events
         xml []]
    (case type
      :start-element
      (let [[_ tag attrs] evt
            [children events] (to-nodes events)]
        (recur events
               (conj xml (Element. tag attrs children))))

      :text
      (recur events (conj xml (evt 1)))

      (:end-element nil)
      (do
        (println "Returning: " xml)
        [xml events])
      
      )))



(defn parse [in]
  (-> in
      event-seq
      merge-text
      ;;(ignore :start-cdata :end-cdata)
      ;;(ignore :start-prefix-mapping :end-prefix-mapping)
      ;;(ignore :comment)
      to-nodes
      ffirst
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emitting XML

(defn ^String xml-escape [^String s]
  (-> s
      (.replaceAll "&"  "&amp;")
      (.replaceAll "<"  "&lt;")
      ))

(defn ^String xml-escape-attr [^String s]
  (-> s
      xml-escape
      (.replaceAll "'"  "&apos;")
      ))

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

