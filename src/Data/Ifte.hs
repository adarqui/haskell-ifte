module Data.Ifte (
  ifte,
  ifet,
  teif,
  etif,
  ifteEither,
  ifetEither,
  teifEither,
  etifEither,
  ifMaybe,
  fiMaybe
) where



import           Data.Either ()
import           Data.Maybe  ()



-- | If True then a else a'
--
ifte :: Bool -> a -> a -> a
ifte True a _   = a
ifte False _ a' = a'



-- | If True then a else a'
--
ifet :: Bool -> a -> a -> a
ifet tf a' a = ifte tf a a'



-- | a if True, else a'
--
teif :: a -> a -> Bool -> a
teif a a' tf = ifte tf a a'



-- | a if True, else a'
--
etif :: a -> a -> Bool -> a
etif a' a tf = ifte tf a a'



-- | If True then (Right a) else (Left b)
--
ifteEither :: Bool -> a -> b -> Either b a
ifteEither True a _  = Right a
ifteEither False _ b = Left b



-- | If True then (Right a) else (Left b)
--
ifetEither :: Bool -> b -> a -> Either b a
ifetEither tf b a = ifteEither tf a b



-- | (Right a) if True, else (Left b)
--
teifEither :: a -> b -> Bool -> Either b a
teifEither a b tf = ifteEither tf a b



-- | (Right a) if True, else (Left b)
--
etifEither :: b -> a -> Bool -> Either b a
etifEither b a tf = ifteEither tf a b



-- | If True, then Just a else Nothing
--
ifMaybe :: Bool -> a -> Maybe a
ifMaybe True  = Just
ifMaybe False = const Nothing



-- | (Just a) if True, else Nothing
--
fiMaybe :: a -> Bool -> Maybe a
fiMaybe a tf = ifMaybe tf a
