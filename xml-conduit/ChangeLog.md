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
