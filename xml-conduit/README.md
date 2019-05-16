## xml-conduit

This package provides parsing and rendering functions for XML. It is based on the datatypes found in the xml-types package. This package is broken up into the following modules:

* Text.XML: DOM-based parsing and rendering. This is the most commonly used module.

* Text.XML.Cursor: A wrapper around `Text.XML` which allows bidirectional traversing of the DOM, similar to XPath. (Note: Text.XML.Cursor.Generic is the same concept, but will work with any node representation.)

* Text.XML.Unresolved: A slight modification to `Text.XML` which does not require all entities to be resolved at parsing. The datatypes are slightly more complicated here, and therefore this module is only recommended when you need to deal directly with raw entities.

* Text.XML.Stream.Parse: Streaming parser, including some streaming parser combinators.

* Text.XML.Stream.Render: Streaming renderer.

Additionally, the [xml-hamlet
package](http://www.stackage.org/package/xml-hamlet) provides a more convenient
syntax for creating XML documents. For a more thorough tutorial on this
library, please see
[http://www.yesodweb.com/book/xml](http://www.yesodweb.com/book/xml).

### Ensuring constant memory usage

If you experience unnecessarily high/increasing memory usage with `Text.XML.Stream.Parse`, you may want to compile `xml-conduit` with `-fno-full-laziness`. You can tell `stack` to use the flag only for this package by putting the following lines into `stack.yaml`:

```
ghc-options:
    xml-conduit: -fno-full-laziness
```
