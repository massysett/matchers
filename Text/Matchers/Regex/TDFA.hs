module Text.Matchers.Regex.TDFA (tdfa) where

import Control.Monad.Exception.Synchronous
  ( Exceptional (Exception))
import qualified Text.Regex.TDFA as TDFA
import qualified Text.Regex.Base.RegexLike as RL

import Text.Matchers.CaseSensitive
  (CaseSensitive(Sensitive, Insensitive))

data StrErr a = Good a
              | Bad String
              deriving (Show, Eq)

instance Monad StrErr where
  return = Good
  (Good a) >>= f = f a
  (Bad s) >>= _ = Bad s
  fail s = Bad s

tdfa :: CaseSensitive -> String -> Exceptional String (String -> Bool)
tdfa c regexStr = case RL.makeRegexOptsM comp exec regexStr of
  (Bad s) -> Exception s
  (Good rx) -> return (RL.matchTest rx)
  where
    comp = RL.defaultCompOpt { TDFA.caseSensitive = case c of
                                  Sensitive -> True
                                  Insensitive -> False
                             , TDFA.newSyntax = True
                             , TDFA.lastStarGreedy = True }
    exec = RL.defaultExecOpt { TDFA.captureGroups = False }

