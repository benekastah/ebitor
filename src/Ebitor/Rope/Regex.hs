{-# LANGUAGE MultiParamTypeClasses #-}
-- Copied from http://hackage.haskell.org/package/regex-tdfa-1.2.0/docs/src/Text-Regex-TDFA-ByteString.html#compile
-- and modified to suit my needs.  Many thanks to Chris Kuklewicz.
module Ebitor.Rope.Regex
    ( Regex
    , CompOption
    , ExecOption
    , compile
    , compileDefault
    , compileFast
    , execute
    , matchAll
    , matchOnce
    , matchOnceEnd
    , matchOnceBefore
    , matchOnceFrom
    , regexec
    , replace
    , replaceCount
    , replaceOne
    ) where

import Data.Array((!),elems)

import Text.Regex.Base(MatchArray,RegexContext(..),RegexMaker(..),RegexLike(..),Extract(..))
import Text.Regex.Base.Impl(polymatch,polymatchM)
import Text.Regex.Base.RegexLike (defaultExecOpt, defaultCompOpt)
import Text.Regex.TDFA.Common(Regex(..),CompOption(lastStarGreedy),ExecOption(captureGroups))
import Text.Regex.TDFA.ReadRegex(parseRegex)
import Text.Regex.TDFA.String() -- piggyback on RegexMaker for String
import Text.Regex.TDFA.TDFA(patternToRegex)

import Data.Maybe(listToMaybe)
import Text.Regex.TDFA.NewDFA.Engine(execMatch)
import Text.Regex.TDFA.NewDFA.Tester as Tester(matchTest)
import Text.Regex.TDFA.NewDFA.Uncons as Uncons(Uncons(..))
import Data.FingerTree ((<|), ViewR((:>)))
import qualified Data.FingerTree as F

import Ebitor.Rope (Rope)
import qualified Ebitor.Rope as R

instance Extract Rope where
    before = R.take
    after = R.drop
    empty = R.empty

instance Uncons Rope where
    uncons = R.uncons

instance RegexContext Regex Rope Rope where
    match = polymatch
    matchM = polymatchM

instance RegexMaker Regex CompOption ExecOption Rope where
    makeRegexOptsM c e source = makeRegexOptsM c e (R.unpack source)

instance RegexLike Regex Rope where
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

compile :: CompOption -- ^ Flags (summed together)
        -> ExecOption -- ^ Flags (summed together)
        -> Rope -- ^ The regular expression to compile
        -> Either String Regex -- ^ Returns: the compiled regular expression
compile compOpt execOpt rope =
    case parseRegex (R.unpack rope) of
        Left err -> Left ("parseRegex for Ebitor.Rope.Regex failed:"++show err)
        Right pattern -> Right (patternToRegex pattern compOpt execOpt)

fastCompOpt = defaultCompOpt { lastStarGreedy = True }
fastExecOpt = defaultExecOpt { captureGroups = False }

compileDefault :: Rope -> Either String Regex
compileDefault = compile defaultCompOpt defaultExecOpt

compileFast :: Rope -> Either String Regex
compileFast = compile fastCompOpt fastExecOpt

execute :: Regex      -- ^ Compiled regular expression
        -> Rope -- ^ Rope to match against
        -> Either String (Maybe MatchArray)
execute r rope = Right (matchOnce r rope)

regexec :: Regex      -- ^ Compiled regular expression
        -> Rope -- ^ Rope to match against
        -> Either String (Maybe (Rope, Rope, Rope, [Rope]))
regexec r rope =
    case matchOnceText r rope of
        Nothing -> Right (Nothing)
        Just (pre,mt,post) ->
            let main = fst (mt!0)
                rest = map fst (tail (elems mt)) -- will be []
            in  Right (Just (pre,main,post,rest))

replaceCount :: Int
             -> Regex
             -> Rope
             -> Rope
             -> Rope
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


replaceOne :: Regex
           -> Rope
           -> Rope
           -> Rope
replaceOne = replaceCount 1

replace :: Regex
        -> Rope
        -> Rope
        -> Rope
replace = replaceCount (-1)


matchOnceFrom :: Regex -> Int -> Rope -> Maybe (Int, Int)
matchOnceFrom regex i rope =
    let r = snd $ R.splitAt i rope
    in  setOffset `fmap` (!0) `fmap` (matchOnce regex r)
  where
    setOffset (offset, len) = (i + offset, len)

matchOnceEnd :: Regex -> Rope -> Maybe (Int, Int)
matchOnceEnd regex (R.Rope rope) = matchOnceEnd' [] rope F.empty
  where
    matchOnceEnd' matches rest rope
        | length matches > 1 || F.null rest =
            setOffset `fmap` (!0) `fmap` listToMaybe (reverse matches)
        | otherwise =
            let rest' :> chunk = F.viewr rest
                rope' = chunk <| rope
            in  matchOnceEnd' (matchAll regex $ R.Rope rope') rest' rope'
      where
        restLen = R.length (R.Rope rest)
        setOffset (offset, len) = (restLen + offset, len)

matchOnceBefore :: Regex -> Int -> Rope -> Maybe (Int, Int)
matchOnceBefore regex i rope =
    let r = fst $ R.splitAt i rope
    in  matchOnceEnd regex r
