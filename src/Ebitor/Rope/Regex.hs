{-# LANGUAGE MultiParamTypeClasses #-}
-- Copied from http://hackage.haskell.org/package/regex-tdfa-1.2.0/docs/src/Text-Regex-TDFA-ByteString.html#compile
-- and modified to suit my needs.  Many thanks to Chris Kuklewicz.
module Ebitor.Rope.Regex
    ( Regex
    , CompOption
    , ExecOption
    , compile
    , compileDefault
    , execute
    , regexec
    , replace
    , replaceCount
    , replaceOne
    ) where

import Data.Array((!),elems)

import Text.Regex.Base(MatchArray,RegexContext(..),RegexMaker(..),RegexLike(..),Extract(..))
import Text.Regex.Base.Impl(polymatch,polymatchM)
import Text.Regex.Base.RegexLike (defaultExecOpt, defaultCompOpt)
import Text.Regex.TDFA.Common(Regex(..),CompOption,ExecOption(captureGroups))
import Text.Regex.TDFA.ReadRegex(parseRegex)
import Text.Regex.TDFA.String() -- piggyback on RegexMaker for String
import Text.Regex.TDFA.TDFA(patternToRegex)

import Data.Maybe(listToMaybe)
import Text.Regex.TDFA.NewDFA.Engine(execMatch)
import Text.Regex.TDFA.NewDFA.Tester as Tester(matchTest)
import Text.Regex.TDFA.NewDFA.Uncons as Uncons(Uncons(..))

import Ebitor.Rope.Part (RopePart)
import Ebitor.Rope.Generic (GenericRope)
import qualified Ebitor.Rope.Generic as R

instance RopePart a => Extract (GenericRope a) where
    before = R.take
    after = R.drop
    empty = R.empty

instance RopePart a => Uncons (GenericRope a) where
    uncons = R.uncons

instance RopePart a => RegexContext Regex (GenericRope a) (GenericRope a) where
    match = polymatch
    matchM = polymatchM

instance RopePart a => RegexMaker Regex CompOption ExecOption (GenericRope a) where
    makeRegexOptsM c e source = makeRegexOptsM c e (R.unpack source)

instance RopePart a => RegexLike Regex (GenericRope a) where
    matchOnce r s = listToMaybe (matchAll r s)
    matchAll r s = execMatch r 0 '\n' s
    matchCount r s = length (matchAll r' s)
      where
        r' = r { regex_execOptions = (regex_execOptions r) {captureGroups = False} }
        matchOnceText regex source =
            fmap (\ma -> let (o,l) = ma!0
                         in  ( R.take o source
                             , fmap (\ol@(off,len) -> (R.take len (R.drop off source),ol)) ma
                             , R.drop (o+l) source))
                         (matchOnce regex source)
    matchAllText regex source =
        map (fmap (\ol@(off,len) -> (R.take len (R.drop off source),ol)))
            (matchAll regex source)

compile :: RopePart a
        => CompOption -- ^ Flags (summed together)
        -> ExecOption -- ^ Flags (summed together)
        -> (GenericRope a) -- ^ The regular expression to compile
        -> Either String Regex -- ^ Returns: the compiled regular expression
compile compOpt execOpt rope =
    case parseRegex (R.unpack rope) of
        Left err -> Left ("parseRegex for Ebitor.Rope.Regex failed:"++show err)
        Right pattern -> Right (patternToRegex pattern compOpt execOpt)

compileDefault :: RopePart a => (GenericRope a) -> Either String Regex
compileDefault = compile defaultCompOpt defaultExecOpt

execute :: RopePart a => Regex      -- ^ Compiled regular expression
        -> (GenericRope a) -- ^ Rope to match against
        -> Either String (Maybe MatchArray)
execute r rope = Right (matchOnce r rope)

regexec :: RopePart a => Regex      -- ^ Compiled regular expression
        -> (GenericRope a) -- ^ Rope to match against
        -> Either String (Maybe (GenericRope a, GenericRope a, GenericRope a, [GenericRope a]))
regexec r rope =
    case matchOnceText r rope of
        Nothing -> Right (Nothing)
        Just (pre,mt,post) ->
            let main = fst (mt!0)
                rest = map fst (tail (elems mt)) -- will be []
            in  Right (Just (pre,main,post,rest))

replaceCount :: RopePart a
             => Int
             -> Regex
             -> (GenericRope a)
             -> (GenericRope a)
             -> (GenericRope a)
replaceCount n r replacement haystack = replaceCount' n ("", haystack)
  where
    replaceCount' 0 (h1, h2) = R.append h1 h2
    replaceCount' n haystack@(h1, h2) =
        case matchOnce r h2 of
            Just match ->
                let (offset, len) = match ! 0
                    (prefix, result) = R.splitAt offset h2
                    h1' = R.concat [h1, prefix, replacement]
                    result' = R.drop len result
                in  replaceCount' (n - 1) (h1', result')
            Nothing -> replaceCount' 0 haystack


replaceOne :: RopePart a
           => Regex
           -> (GenericRope a)
           -> (GenericRope a)
           -> (GenericRope a)
replaceOne = replaceCount 1

replace :: RopePart a
        => Regex
        -> (GenericRope a)
        -> (GenericRope a)
        -> (GenericRope a)
replace = replaceCount (-1)
