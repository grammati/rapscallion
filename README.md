# Rapscallion #

Rapscallion is an XML template language for Clojure, primarily
inspired by Genshi (http://genshi.edgewall.org).

A rapscallion template is XML with processing commands embedded,
usually as attributes. It aims to be useful for both simple templating
tasks, as well as complex XML generation and transformation by
combining the strengths of a template language with those of an XML
transformation language (eg: XSLT).

Let's start with an example.

## Basic Example ##

From here on, I'll assume you have done this:

```clojure
    (require '(rapscallion (core :as rap)))
```

Here is the simplest possible example of using a Rapscallion template:

```clojure
    (rap/render "<foo/>")
    #=> "<foo/>"
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
    (def my-template "<foo rap:args='thing'>$thing</foo>")
    (rap/render my-template {:thing "Hello"})
    #=> "<foo>Hello</foo>"
    (rap/render my-template {:thing "Bye"})
    #=> "<foo>Bye</foo>"
```

This demonstrates the first useful feature - substituting values into
the template. Note that unlike many other template languages,
Rapscallion requires that you declare the arguments that you expect to
have passed in, in the form of a rap:args attribute on the root
element.

Substitutions can appear in text or in attributes. They consist of a $
character followed by a single clojure expression, optionally wrapped
in braces. To illustrate:

```clojure
    (rap/render 
      "<foo rap:args='a b c'>
         <bar id='${a}_id' name='$b'>thing $(or b c)</bar>
       </foo>"
      {:a 3 
       :c "three"})
    #=> "<foo><bar id='3_id'>thing three</bar></foo>"
```

In this example, the template accepts 3 inputs, a, b, and c. In the id
attribute, braces are needed around the substitution. Without them, we
would get an error because there is no symbol `a_id` defined.

Note also that we have omitted `b` in the actual call to the template,
so it gets the value `nil`. Because the `name` attribute has a nil
value, it is omitted completely from the rendered output.


