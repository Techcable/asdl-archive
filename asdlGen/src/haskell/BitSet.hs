module BitSet (andb,orb, shiftr, shiftl, notb ) where

type CHAR = Char
type PString = String -- "persistent" string needs to be explicitly freed
-- Note: we do not consider NULL to be a valid pointer value.
-- Anything that requires a NULL pointer of type "T" should be
-- modified to require a "Maybe T" and an appropriate "Maybe"
-- declaration defined.
newtype Ptr = Ptr Int deriving (Eq)

unPtr :: Ptr -> Int
unPtr (Ptr i) = i

plusPtr  :: Ptr -> Int -> Ptr
plusPtr  p x = Ptr (unPtr p + x)

minusPtr :: Ptr -> Ptr -> Int
minusPtr p q = unPtr p - unPtr q
-- This is used in restricted type synonyms where there are no
-- Haskell functions which need to "look inside the representation".
-- It overcomes a (normally reasonable) restriction that the type be
-- "open" in a non-empty list of variables.
rtsDummy = error "rtsDummy"
type Word8 = Int
type Word16 = Int
type Word32 = Int
type Word = Int
type Int8 = Int
type Int16 = Int
type Int32 = Int
type Int64 = Int
type Word64 = Int
class BitSet a where
 toWord32 :: a -> Word32
 fromWord32 :: Word32 -> a

-- really want a Word32 instance - but that's a synonym for Int so...
instance BitSet Int where
 toWord32   = id
 fromWord32 = id

orb, andb :: BitSet a => a -> a -> a
orb x y = fromWord32 (orWord32 (toWord32 x) (toWord32 y))
andb x y = fromWord32 (andWord32 (toWord32 x) (toWord32 y))

notb :: BitSet a => a -> a
notb x = fromWord32 (notWord32 (toWord32 x))

emptyb, fullb :: BitSet a => a
emptyb = fromWord32 emptyWord32
fullb  = fromWord32 fullWord32

{- Sigh, I ought to know better by now... not legal Haskell
instance BitSet a => Eq a where
  x == y = toWord32 x == toWord32 y
-}

eqb :: BitSet a => a -> a -> Bool
x `eqb` y =  toWord32 x `eqWord32` toWord32 y

isEmptyb :: BitSet a => a -> Bool
isEmptyb x = x `eqb` emptyb

elemb :: BitSet a => Int -> a -> Bool
x `elemb` y = x `elemWord32` toWord32 y

shiftl, shiftr :: BitSet a => a -> Int -> a
shiftl x y = fromWord32 (shiftlWord32 (toWord32 x) y)
shiftr x y = fromWord32 (shiftrWord32 (toWord32 x) y)

orbs, andbs :: BitSet a => [a] -> a
orbs  = foldl orb  emptyb
andbs = foldl andb fullb
primitive orWord32 "XS_orWord32" :: Word32 -> Word32 -> Word32
primitive andWord32 "XS_andWord32" :: Word32 -> Word32 -> Word32
primitive notWord32 "XS_notWord32" :: Word32 -> Word32
emptyWord32 :: Word32
emptyWord32 = consts_Word32 0
fullWord32 :: Word32
fullWord32 = consts_Word32 1
primitive eqWord32 "XS_eqWord32" :: Word32 -> Word32 -> Bool
primitive elemWord32 "XS_elemWord32" :: Int -> Word32 -> Bool
primitive shiftlWord32 "XS_shiftlWord32" :: Word32 -> Int -> Word32
primitive shiftrWord32 "XS_shiftrWord32" :: Word32 -> Int -> Word32
primitive consts_Word32 "XSconsts_Word32" :: Int -> Word32

needPrims_hugs

