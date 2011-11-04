module Text.HTML.TagStream.Utils where
import Data.Word
import Foreign.ForeignPtr       (ForeignPtr, withForeignPtr)
import Foreign.Storable         (Storable(..))
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S

peekByte p i = S.inlinePerformIO $ withForeignPtr p $ \p' -> peekByteOff p' i

cons' :: Word8 -> S.ByteString -> S.ByteString
cons' c bs@(S.PS p s l)
  | s>0 && peekByte p (s-1)==c = S.PS p (s-1) (l+1)
  | otherwise = S.cons c bs

cons = cons' . S.c2w

append :: S.ByteString -> S.ByteString -> S.ByteString
append (S.PS p1 s1 l1) (S.PS p2 s2 l2)
  | p1==p2 && (s1+l1)==s2 = S.PS p1 s1 (l1+l2)
append xs ys = S.append xs ys
