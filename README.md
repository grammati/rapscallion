# Rapscallion #

Rapscallion is an XML template language for Clojure, primarily
inspired by Genshi (http://genshi.edgewall.org).

A rapscallion template is XML with processing commands embedded,
usually as attributes. Let's start with an example.

## Basic Example ##

Template file, "tmpl-1.rap.xml":

<foo rap:args="things">
  <bar rap:for="thing things">$thing</bar>
</foo>

Clojure file:

(use '(rapscallion [core :as rap]))
(println (rap/render "/path/to/tmpl1.rap.xml" {:things ["one" "two" "three"]}))

Result:

<foo>
  <bar>one</bar>
  <bar>two</bar>
  <bar>three</bar>
</foo>

## Notes ##

* Template files must be well-formed XML.
* $foo or ${foo} is a subsitution, where foo can be any (single) clojure form.
* $foo substitutions can appear in text or in attibute values (eg: not as tagnames)
* processing directives go in attributes prefixed with rap:
* built-in directives include for, if, let, match, defn, and more (details follow)

## More Notes ##

* rap:use, rap:require, rap:import...  on root element only?
* How to use template-defns defined in other files? rap:include="..."?
* Root of a template file can be:
  * <rap:template args="..."
  * <foo rap:args="..."
  * <rap:template-lib>
  * 



