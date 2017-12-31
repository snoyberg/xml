## 1.7.0.1

* Use `throwM` instead of `monadThrow`

## 1.7.0

* `psDecodeEntities` is no longer passed numeric character references (e.g., `&#x20;`, `&#65;`) and the predefined XML entities (`&amp;`, `&lt;`, etc). They are now handled by the parser. Both of these construct classes only have one spec-compliant interpretation and this behaviour must always be present, so it makes no sense to force user code to re-implement the parsing logic.
* In prior versions of xml-conduit, hexadecimal character references with a leading `0x` or `0X` like `&0x20;` were accepted. This was not in compliance with the XML specification and it has been corrected.
* xml-conduit now rejects some (but not all) invalid-according-to-spec entities during parsing: specifically, entities with a leading `#` that are not character references are no longer allowed and will be parse errors.

## 1.6.0

* Dropped the dependency on `data-default` for `data-default-class`, reducing the transitive dependency load. For most users, this will not be a breaking change, but it does mean that importing `Text.XML.Conduit` will no longer bring various instances for `Default` into scope. This will break code that relies on those instances and does not otherwise see them. To fix this, import `Data.Default` from `data-default` or one of the more specific instance-providing packages directly (e.g., `data-default-dlist` for the `DList` instance).

## 1.5.1

* New render setting, `rsXMLDeclaration`; setting it to `False` omits the XML declaration.

## 1.5.0

* `tag` function no longer throws an exception when attributes don't match [#93](https://github.com/snoyberg/xml/pull/93)
* Add `many_` combinator to avoid building results in memory [#94](https://github.com/snoyberg/xml/pull/94)
* Turn some functions from `Consumer Event m a` to `ConduitM Event o m a` to allow yielding values
* Replace `takeAllTreesContent` with `takeAnyTreeContent`, that only consumes one tree
* Introduce `NameMatcher` type to refactor tag parsers
* Add a couple of `take*` functions to stream events rather than parse them
* Rename `ignore*` functions to comply with naming convention

## 1.4.0.3

* Compatibility with blaze-markup-0.8.0.0 [#95](https://github.com/snoyberg/xml/issues/95)

## 1.4.0.2

* Parse XML encoding case-insensitively
* Remove extra EOL when printing XmlException

## 1.4.0.1

* Handle CDATA in takeAllTreesContent [#88](https://github.com/snoyberg/xml/pull/88)

## 1.4.0

* Improve XmlException definition and usage
* Add 'takeAllTreesContent' function

## 1.3.5

* Improvements for using xml-conduit for streaming XML protocols [#85](https://github.com/snoyberg/xml/pull/85)

## 1.3.4.2

* transformers dep bump

## 1.3.4.1

* Remove unneeded ImpredicativeTypes

## 1.3.4

* dropWS retains consumed whitespace values [#74](https://github.com/snoyberg/xml/issues/74) [#75](https://github.com/snoyberg/xml/pull/75) [#76](https://github.com/snoyberg/xml/pull/76)

## 1.3.3.1

* Generalize signature of choose (Fixes [#72](https://github.com/snoyberg/xml/issues/72)) [#73](https://github.com/snoyberg/xml/pull/73)

## 1.3.3

* New render setting to control when to use CDATA [#68](https://github.com/snoyberg/xml/pull/68)
* Escaping CDATA closing tag in CDATA [#69](https://github.com/snoyberg/xml/pull/69)

## 1.3.2

* Support for iso-8859-1 [#63](https://github.com/snoyberg/xml/issues/63)

## 1.3.1

* Add functions to ignore subtrees & result-streaming (yield) parsers [#58](https://github.com/snoyberg/xml/pull/58)

## 1.3.0

* Drop system-filepath

## 1.2.6

* Reuse 'MonadThrow' and 'force' for 'AttrParser' [#52](https://github.com/snoyberg/xml/pull/52)

## 1.2.5

*  Added helper functions to render XML elements [#48](https://github.com/snoyberg/xml/pull/48)

## 1.2.4

* 'parseText' becomes 'parseText'/'parseTextPos', depending on the output type [#47](https://github.com/snoyberg/xml/pull/47)

## 1.2.3.3

* Allow blaze-builder 0.4

## 1.2.3.2

* Doc fix [#44](https://github.com/snoyberg/xml/pull/44)

## 1.2.3.1

Support monad-control 1.0
