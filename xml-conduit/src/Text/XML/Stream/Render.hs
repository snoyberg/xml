{- | 'Enumeratee's to render XML 'Event's. Unlike libxml-enumerator and
expat-enumerator, this module does not provide IO and ST variants, since the
underlying rendering operations are pure functions.
-}
module Text.XML.Stream.Render (
    -- * Rendering XML files
    renderBuilder,
    renderBuilderFlush,
    renderBytes,
    renderText,

    -- * Renderer settings
    RenderSettings,
    def,
    rsNamespaces,
    rsAttrOrder,
    rsUseCDATA,
    rsXMLDeclaration,
    orderAttrs,

    -- * Event rendering
    tag,
    content,

    -- * Attribute rendering
    Attributes,
    attr,
    optionalAttr,
) where

import Text.XML.Stream.Render.Internal
