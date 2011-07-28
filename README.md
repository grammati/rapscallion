# Rapscallion #

Rapscallion is an XML template language for Clojure, primarily
inspired by Genshi (http://genshi.edgewall.org).

A rapscallion template is XML with processing commands embedded,
usually as attributes. It aims to be useful for both simple templating
tasks, as well as complex XML generation and transformation by
combining the strengths of a template language with those of an XML
transformation language (eg: XSLT).

## For the Impatient ##

Template:

```xml
    <!-- my-template.xml -->
    <table rap:args='data headers'                <!-- parameters to be passed in -->
           rap:require='(clojure [string :as s])' <!-- use any clojure functions you like -->
           >
      <tr rap:if='headers'>                       <!-- conditionally include elements -->
        <th rap:for='title headers'>              <!-- repeat elements -->
          $(s/upper-case title)                   <!-- embed arbitrary clojure expressions -->
        </th>
      </tr>
      <tr rap:for='{:keys [id name]} data'        <!-- syntax inside directives is normal clojure -->
          rap:if='(not-empty name)'               <!-- multiple directives allowed per-element -->
          id='row_$id'                            <!-- substitutions in attributes... -->
          >
        <td>$id</td>                              <!-- ... or in text -->
        <td>$name</td>
      </tr>
    </table>
```

Code:

```clojure
    (ns my.ns
      (:require (rapscallion [core :as rap])))

    (rap/render 
      "my-template.xml"
      {:data [{:id 1 :name "Annie"}
              {:id 2 :name "Bobby"}
              {:id 3 :name nil}]
       :headers ["id" "name"]})
```

Result:

```xml
    <table>
      <tr>
        <th>ID</th>
        <th>NAME</th>
      </tr>
      <tr id='row_1'>
        <td>1</td>
        <td>Annie</td>
      </tr>
      <tr id='row_2'>
        <td>2</td>
        <td>Bobby</td>
      </tr>
    </table>
```


## Tutorial ##

From here on, I'll assume you have done this:

```clojure
    (require '(rapscallion (core :as rap)))
```

Here is the simplest possible example of using a Rapscallion template:

```clojure
    (rap/render "<foo/>")  ; "<foo/>"
```

Not very impressive so far, but it illustrates two things:

 * rap/render - this function takes a template as input, and returns a
string containing the rendered output. The argument can be a string of
XML, or a filename (actually, anything that clojure.java.io/reader
will accept).
 * Any well-formed XML is a valid Rapscallion template. In this
example, the XML contains no special Rapscallion directives, so
rendering it simply returns it as-is.

### Substitutions ###

Here is a slightly less trivial example:

```clojure
    (def my-template "<foo rap:args='thing'>$thing</foo>")
    (rap/render my-template {:thing "Hello"})  ; "<foo>Hello</foo>"
    (rap/render my-template {:thing "Bye"})    ; "<foo>Bye</foo>"
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
    ;; "<foo><bar id='3_id'>thing three</bar></foo>"
```

In this example, the template accepts 3 inputs, a, b, and c. In the id
attribute, braces are needed around the substitution. Without them, we
would get an error because the symbol `a_id` is not defined.

Note also that we have omitted `b` in the actual call to the template,
so it gets the value `nil`. Because the `name` attribute has a nil
value, it is omitted completely from the rendered output.


### Directives ###

To anything really interesting with a rapscallion template, you use
_directives_. Directives are processing commands embedded in the XML,
either as attributes or as elements.

One of the first directives you'll want to use is `for`.

```clojure
    (rap/render 
      "<foo rap:args='col'>
         <bar rap:for='item col'>
           $item
         </bar>
       </foo>"
      {:col ["one" "two" "three"]})
    ;; "<foo><bar>one</bar><bar>two</bar><bar>three</bar></foo>"
```

The example above is exactly equivalent to:

```clojure
    (rap/render 
      "<foo rap:args='col'>
         <rap:for bindings='item col'>
           <bar>
             $item
           </bar>
         </rap:for>
       </foo>"
      {:col ["one" "two" "three"]})
    ;; "<foo><bar>one</bar><bar>two</bar><bar>three</bar></foo>"
```

####
NOTE: Everything from here down is a work-in-progress.

TODO : basic directives (if, let, ...)

### Defns ###

Define functions, call them, call-with-content.


### Matchers ###

Define matchers. Match-expression syntax. Scope of a match. Match actions (replace, append-child, ...)


### Includes ###

How to split up you template into a library of defns and matchers. How to reuse them. How to use filter-templates programmatically.


### Code blocks ###

Putting code in a <?clojure ... ?> processing instruction.


### Extending ###

Extend the behavior of rapscallion.

#### Defining substitutions ####

xml-value multimethod/protocol applied to substitutions

#### defdirective ####

defdirective to define your own rap:xxx directives


### Template Loaders ###

What they are, how they work, why...

#### Caching ####

Caching of compiled templates, reloading strategies.


### Compilation ###

Describe how a template is compiled into clojure code and evaled.

