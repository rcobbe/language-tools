module LT.Latin.Letter where

data Letter = Letter { base :: Char, macron :: Macron }
            deriving (Eq, Show)

data Macron = NoMacron | Macron
  deriving (Eq, Show, Ord)

instance Ord Letter where
  compare l1 l2 =
    compareSeries [compare (base l1) (base l2),
                   compare (macron l1) (macron l2)]

compareSeries :: [Ordering] -> Ordering
compareSeries = foldl' update EQ
 where update EQ x    = x
       update accum _ = accum
