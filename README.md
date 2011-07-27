# Rapscallion #

Rapscallion is an XML template language for Clojure, primarily
inspired by Genshi (http://genshi.edgewall.org).

A rapscallion template is XML with processing commands embedded,
usually as attributes. Let's start with an example.

## Basic Example ##

From here on, I'll assume you have done this:

```clojure
    (require '(rapscallion (core :as rap)))
```

Here is the simplest possible example of using a Rapscallion template:

```clojure
    user> (rap/render "<foo/>")
    "<foo/>"
```

Not very impressive so far, but it illustrates two things:
* rap/render - this function takes a template as input, and returns a
string containing the rendered output. The argument can be a string of
XML, or a filename (actually, anything that clojure.java.io/reader
will accept).
* Any well-formed XML is a valid Rapscallion template. In this
example, the XML contains no special Rapscallion directives, so
rendering it simply returns it as-is.

Here is a slightly less trivial example:

```clojure
    user> (def my-template "<foo rap:args='thing'>$thing</foo>")
    #'user/my-template
    user> (rap/render my-template {:thing "Hello"})
    "<foo>Hello</foo>"
    user> (rap/render my-template {:thing "Bye"})
    "<foo>Bye</foo>"
```

Template file, "tmpl-1.rap.xml":

```xml
    <foo rap:args="things">
      <bar rap:for="thing things">$thing</bar>
    </foo>
```




