module Horbits.Rematch where

import Control.Lens (Getting, (^.))
import           Control.Rematch
import           Control.Rematch.Formatting
import           Control.Rematch.Run
import           Test.QuickCheck

--has :: (String, b -> a) -> Matcher a -> (String, Matcher b)
--has (n, f) m = (n, on m (f, n))

has :: (String, Getting a s a) -> Matcher a -> (String, Matcher s)
has (n, g) m = (n, on m ((^. g), n))

always :: Bool -> Matcher a
always b = Matcher (const b) ("const" ++ show b) (const $ "always " ++ show b)


allOf' :: [(String, Matcher a)] -> Matcher a
allOf' nm = Matcher {
    match = and . fmap isMatch . matches,
    description = describeList "all" $ map (description . snd) nm,
    describeMismatch = join "," . foldr consMatch [] . matches
  } where
    matches a = fmap (\ (n, m) -> (n, match m a, describeMismatch m a)) nm
    consMatch (_, True, _) = id
    consMatch (n, False, f) = ((n ++ " " ++ f) :)
    isMatch (_, x, _) = x

matcherProperty :: Matcher a -> a -> Property
matcherProperty m x = case runMatch m x of MatchSuccess -> property True
                                           MatchFailure f -> counterexample f $ property False


