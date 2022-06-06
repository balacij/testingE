{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}

module Lib
  ( someFunc,
    QuantityDict (..),
    numSpaces,
    Space (..),
    SpaceM,
    Expr1 (..),
    space1,
    Expr1b,
    qd1b,
    int1b,
    dbl1b,
    add1b,
    Expr2 (..),
    space2,
    Expr2b (..),
    space2b,
  )
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{------------------------------------------------------------------------------
-- Spaces
------------------------------------------------------------------------------}

data Space = IntS | DblS | StringS
  deriving (Eq, Show)

type SpaceM = Maybe Space

numSpaces :: [Space]
numSpaces = [IntS, DblS]

{------------------------------------------------------------------------------
-- Symbols / QuantityDicts
------------------------------------------------------------------------------}

data QuantityDict = QD
  { _symbol :: String,
    _space :: Space
  }

{------------------------------------------------------------------------------
-- (Option 1a)
--
-- Expression language allowing invalid expressions, with optional validity
-- checking performed on demand
------------------------------------------------------------------------------}

data Expr1 where
  QD1 :: QuantityDict -> Expr1
  Int1 :: Int -> Expr1
  Dbl1 :: Double -> Expr1
  Add1 :: Expr1 -> Expr1 -> Expr1

space1 :: Expr1 -> SpaceM
space1 (QD1 (QD _ s)) = Just s
space1 (Int1 _) = Just IntS
space1 (Dbl1 _) = Just DblS
space1 (Add1 l r) = do
  l' <- space1 l
  r' <- space1 r
  if l' == r' && elem l' numSpaces then Just l' else Nothing

x :: SpaceM
x = space1 (Add1 (Dbl1 10) (QD1 (QD "a" DblS)))

xBad :: SpaceM
xBad = space1 (Add1 (Int1 10) (QD1 (QD "a" DblS)))

-- >>> x
-- Just DblS

-- >>> xBad
-- Nothing

{------------------------------------------------------------------------------
-- (Option 1b)
--
-- Expression language not allowing invalid expressions, but not enforced at
-- compile-time
------------------------------------------------------------------------------}

data Expr1b where
  QD1b :: QuantityDict -> Expr1b
  Int1b :: Int -> Expr1b
  Dbl1b :: Double -> Expr1b
  Add1b :: Expr1b -> Expr1b -> Expr1b

qd1b :: QuantityDict -> Expr1b
qd1b = QD1b

int1b :: Int -> Expr1b
int1b = Int1b

dbl1b :: Double -> Expr1b
dbl1b = Dbl1b

add1b :: Expr1b -> Expr1b -> Maybe Expr1b
add1b l r = if lS == rS && lS `elem` numSpaces then Just $ Add1b l r else Nothing
  where
    lS = space1b l
    rS = space1b r

space1b :: Expr1b -> Space
space1b (QD1b (QD _ s)) = s
space1b (Int1b _) = IntS
space1b (Dbl1b _) = DblS
space1b (Add1b l _) = space1b l

xB :: SpaceM
xB = space1b <$> add1b (dbl1b 10) (qd1b (QD "b" DblS))

xBBad :: SpaceM
xBBad = space1b <$> add1b (int1b 10) (qd1b (QD "b" DblS))

-- >>> xB
-- Just DblS

-- >>> xBBad
-- Nothing

{------------------------------------------------------------------------------
-- (Option 2a)
--
-- Expression language not allowing invalid expressions at all, piggybacking on
-- Haskell for ensuring validity of expressions at time of creation, without
-- adding typing to QuantityDicts.
------------------------------------------------------------------------------}

data Expr2 t where
  QD2 :: QuantityDict -> Expr2 t
  Int2 :: Int -> Expr2 Int
  Dbl2 :: Double -> Expr2 Double
  Add2 :: Num t => Expr2 t -> Expr2 t -> Expr2 t

-- space2 calculates the Space, which is guaranteed to 'exist' for Expr2
space2 :: Expr2 t -> Space
space2 (QD2 (QD _ s)) = s -- does not necessarily 'line up' with the type parameter filled!
space2 (Int2 _) = IntS
space2 (Dbl2 _) = DblS
space2 (Add2 l _) = space2 l

y :: Space
y = space2 (Add2 (Dbl2 10) (QD2 (QD "c" DblS)))

-- Since "QD2" allows the type parameter to be anything, we can also place an
-- IntS and have no issue at compile-time
yBad :: Space
yBad = space2 (Add2 (Dbl2 10) (QD2 (QD "c" IntS)))

-- >>> y
-- DblS

-- >>> yBad
-- DblS


{------------------------------------------------------------------------------
-- (Option 2b)
--
-- Expression language not allowing invalid expressions at all, piggybacking on
-- Haskell for ensuring validity of expressions at time of creation, with
-- adding typing to QuantityDicts.
------------------------------------------------------------------------------}

-- NOTE: QuantityDictV2 would **require** us to have the Merged Chunk Maps finished
--       ( i.e., #2873 - https://github.com/JacquesCarette/Drasil/issues/2873 )
data QuantityDictV2 t = QDv2 {
    _symbol2 :: String,
    _space2 :: Space
}

-- No guarantee that "t" lines up with the "Space" value, so the same issue
-- occurs, just in a slightly different area/way
qdV2 :: String -> Space -> QuantityDictV2 t
qdV2 = QDv2

-- Now, "QD2b", the constructor, does not allow merging
data Expr2b t where
  QD2b :: QuantityDictV2 t -> Expr2b t
  Int2b :: Int -> Expr2b Int
  Dbl2b :: Double -> Expr2b Double
  Add2b :: Num t => Expr2b t -> Expr2b t -> Expr2b t

space2b :: Expr2b t -> Space
space2b (QD2b (QDv2 _ s)) = s -- again, s does not necessarily 'line up' with the type parameter filled!
space2b (Int2b _) = IntS
space2b (Dbl2b _) = DblS
space2b (Add2b l _) = space2b l

yB :: Space
yB = space2b (Add2b (Dbl2b 10) (QD2b (qdV2 "c" DblS)))

yBBad :: Space
yBBad = space2b (Add2b (Dbl2b 10) (QD2b (qdV2 "c" IntS)))

-- Realistically, we would have symbols manually written
-- So we would have to remember to never write: `realisticQD :: QuantityDictV2 t`
-- but to always write something along the lines of:
realisticQD :: QuantityDictV2 Double
realisticQD = qdV2 "c" DblS

badButRealisticQD :: QuantityDictV2 Double
badButRealisticQD = qdV2 "c" IntS -- does not line up with type signature

-- >>> yB
-- DblS

-- >>> yBBad
-- DblS

-- >>> space2b (Add2b (Dbl2b 10) (QD2b realisticQD))
-- DblS

-- >>> space2b (Add2b (Dbl2b 10) (QD2b badButRealisticQD))
-- DblS

