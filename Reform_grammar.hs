{-
Reform, a decompiler for Z-machine story files.
Copyright 2004 Ben Rudiak-Gould.

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You can read the GNU General Public License at this URL:
     http://www.gnu.org/copyleft/gpl.html
-}


module Reform_grammar (
	GrammarLine(..), GrammarToken(..),
	grammarType, verbs, verbNums,
	actionRoutines, preactionRoutines,
	DictWordTag(..),
	kbdInputCodes, dictionary,
	dictWordAt, isDictWord,
	getInfocomAdjectiveNum
) where


import Reform_storyfile
import Reform_symfile
import Reform_zscii

import Control.Monad (when,liftM,liftM2,replicateM)
import Control.Monad.State (evalState)
import Data.Array (Array,listArray,accumArray,(!),elems,bounds)
import Data.Bits (shiftR,testBit,(.&.),(.|.))
import Data.Ix (range)
import Data.Maybe (isJust,fromMaybe)
import Numeric (showHex)


--                             words after verb action reverse
data GrammarLine = GrammarLine [GrammarToken]   Int    Bool
  deriving Show


verbs :: [([String],[GrammarLine])]
verbNums :: Array Int [String]

verbs = reverse (zip (reverse (elems verbNums)) grammarEntries)

verbNums =
  accumArray (flip (:)) [] (0,255)
   [(n,x) | (x,_,tags) <- reverse (elems dictionary), DictVerb n <- tags]


preps :: Array Int [String]

preps = accumArray (flip (:)) [] (0,255)
          [(n,s) | (s,_,tags) <- elems dictionary, DictPrep n <- tags]


grammarEntries :: [[GrammarLine]]
actionRoutines, preactionRoutines :: [Int]

(grammarEntries, actionRoutines, preactionRoutines) =
  evalFrom hdrStaticMemoryBaseAddr grammarParser


data GrammarToken = ElementaryToken String | Preposition String
                  | Attribute Int | ParseRoutine String Int
                  | InfocomObject Int Int | Alternatives [GrammarToken]
  deriving (Show,Eq,Ord)


{-----------}


-- Heuristics to determine grammar table type taken from InfoDump, except
-- that I disambiguate GV1 and GV2 in a slightly different way (maybe
-- better, maybe worse, definitely easier to code).

(grammarType,grammarParser,dictDataParser) =
  case compiler of
    Inform6  -> parserType_inform6
    Inform5  -> ("Inform5", parseGV1 False, parseDictGV1)	-- parseDictGV1???
    Zilch
      | ver [6]    -> ("InfocomV6", parseInfocomV6, parseDictInfocomV6)
      | otherwise  ->
         let firstEntry  = wordAt hdrStaticMemoryBaseAddr
             secondEntry = wordAt (hdrStaticMemoryBaseAddr+2)
             entryCount  = byteAt firstEntry
         in
           if (secondEntry - firstEntry - 1) `div` entryCount <= 7 then
             ("InfocomVariable", parseInfocomVariable, parseDictInfocom)
           else
             ("InfocomFixed", parseInfocomFixed, parseDictInfocom)

parserType_inform6 =
  let firstEntry = wordAt hdrStaticMemoryBaseAddr
      entryBlock = fromTo hdrStaticMemoryBaseAddr firstEntry
      allEntries = evalState (repeatUntilEmpty getUWord) entryBlock
      entrySizes = zipWith (-) (tail allEntries) allEntries
      isGV1      = all mightBeGV1 entrySizes
      isGV2      = all mightBeGV2 entrySizes
      mightBeGV1 n = n `mod` 8 == 1
      mightBeGV2 n = n `mod` 3 == 1
  in
    if null allEntries then
      ("No grammar", return ([],[],[]), parseDictGV1)
    else if isGV1 && isGV2 then
      error "Unable to disambiguate between Inform 6 GV1 and GV2"
    else if isGV1 then
      ("GV1", parseGV1 True, parseDictGV1)
    else if isGV2 then
      ("GV2", parseGV2, parseDictGV1)
    else
      error "Unable to determine Inform 6 grammar type"


{---------------}


parseInfocomFixed =
  do entries <- getGrammarTable parseInfocomFixedLine
     let maxActionIndex = maximum [i | GrammarLine _  i _ <- concat entries]
     skipZeroes
     actionRoutines    <- replicateM (maxActionIndex+1) getUWord
     preactionRoutines <- replicateM (maxActionIndex+1) getUWord
     return (entries, actionRoutines, preactionRoutines)

parseInfocomFixedLine =
  do objCount   <- getUByte
     prepIndex1 <- getPrepIndex
     prepIndex2 <- getPrepIndex
     a          <- getUByte
     b          <- getUByte
     c          <- getUByte
     d          <- getUByte
     let object1 = InfocomObject a c
         object2 = InfocomObject b d
     actionNum  <- getUByte
     let toks = case objCount of
                  0 -> []
                  1 -> [object1]
                  2 -> object1 : maybePrep prepIndex2 [object2]
                  _ -> error "Grammar table error: object count too high"
     return $ GrammarLine (maybePrep prepIndex1 toks) actionNum False


getPrepIndex =
  do p <- getUByte
     return (if p < 0x80 then 0 else p)


maybePrep 0 rest = rest
maybePrep p rest = prepositionInt p : rest


prepositionInt n =
  case preps ! n of
    [x] -> Preposition x
    xs  -> Alternatives (map Preposition xs)


{---------------}


parseInfocomVariable =
  do entries <- getGrammarTable parseInfocomVariableLine
     let maxActionIndex = maximum [i | GrammarLine _  i _ <- concat entries]
     skipZeroes
     actionRoutines    <- replicateM (maxActionIndex+1) getUWord
     preactionRoutines <- replicateM (maxActionIndex+1) getUWord
     return (entries, actionRoutines, preactionRoutines)

parseInfocomVariableLine =
  do byte      <- getUByte
     let objCount   = byte `shiftR` 6
         prepIndex1 = byte .&. 63
     actionNum <- getUByte
     toks      <- parseInfocomVariableLine' objCount
     return $ GrammarLine (maybePrep' prepIndex1 toks) actionNum False

parseInfocomVariableLine' 3 = error "Grammar table error: object count too high"

parseInfocomVariableLine' objCount =
  do if objCount == 0 then return [] else do
     object1   <- getInfocomObject
     if objCount == 1 then return [object1] else do
     byte      <- getUByte
     let prepIndex2 = byte .&. 63
     object2   <- getInfocomObject
     return (object1 : maybePrep' prepIndex2 [object2])


maybePrep' 0 rest = rest
maybePrep' p rest = prepositionInt (p .|. 0xC0) : rest


getInfocomObject = liftM2 InfocomObject getUByte getUByte


{---------------}


parseGV1 inform6 =
  do entries <- getGrammarTable parseGV1Line
     let maxActionIndex =
           maximum [i | GrammarLine _  i _ <- concat entries]
         maxParseIndex
           | inform6   = maximum [i | GrammarLine ts _ _ <- concat entries, ParseRoutine _ i <- ts]
           | otherwise = maxActionIndex
     skipZeroes
     actionRoutines <- replicateM (maxActionIndex+1) getUWord
     parseRoutines  <- replicateM (maxParseIndex+1) getUWord
     return (map (map (fixupGV1Routines parseRoutines)) entries, actionRoutines, [])

fixupGV1Routines parseRoutines (GrammarLine tokens action reverse) =
  GrammarLine (map helper tokens) action reverse where
    helper (ParseRoutine prefix n) = ParseRoutine prefix (parseRoutines !! n)
    helper (Alternatives tokens)   = Alternatives (map helper tokens)	-- probably unnecessary
    helper x                       = x

parseGV1Line =
  do tokenCount <- getUByte
     tokenArea  <- getBytes 6
     let tokenBytes = evalState (getGV1Tokens tokenCount) tokenArea
         tokens     = map parseGV1TokenByte tokenBytes
     actionNum  <- getUByte
     return $ GrammarLine tokens actionNum False

getGV1Tokens left =
  do eos   <- isEOS
     if eos then return [] else do
     token <- getUByte
     if left == 0 && token < 176
       then (if token == 0 then return []
                           else error "bad GV1 token count")
       else do
     rest <- getGV1Tokens (if token < 176 then (left-1) else left)
     return (token : rest)


parseGV1TokenByte t
  | t < 9     = ElementaryToken (elementaryTokenTypes !! t)
  | t < 16    = error ("Illegal token value " ++ show t)
  | t < 48    = ParseRoutine "noun=" (t-16)	-- FIXME: index into table
  | t < 80    = ParseRoutine "" (t-48)		-- FIXME: index into table
  | t < 112   = ParseRoutine "scope=" (t-80)	-- FIXME: index into table
  | t < 128   = error ("Illegal token value " ++ show t)
  | t < 176   = Attribute (t-128)
  | otherwise = prepositionInt t		-- FIXME: index into table?


{---------------}


parseGV2 =
  do entries <- getGrammarTable parseGV2Line
     let maxActionIndex = maximum [i | GrammarLine _  i _ <- concat entries]
     skipZeroes
     actionRoutines <- replicateM (maxActionIndex+1) getUWord
     return (entries, actionRoutines, [])

parseGV2Line = do
  actionWord <- getUWord
  let actionNum     = actionWord .&. 0x3FF
      reverseParams = testBit actionWord 10
  tokens <- parseGV2Tokens
  return $ GrammarLine (groupGV2Tokens tokens) actionNum reverseParams

parseGV2Tokens = do
  tokenType <- getUByte
  if tokenType == 15
    then return []
    else do
      tokenData <- getUWord
      tokens <- parseGV2Tokens
      return ((tokenType .&. 0x30,parseGV2Token (tokenType .&. 15) tokenData) : tokens)

parseGV2Token 1 d = ElementaryToken (elementaryTokenTypes !! d)
parseGV2Token 2 d = Preposition (dictWordAt d)
parseGV2Token 3 d = ParseRoutine "noun=" d
parseGV2Token 4 d = Attribute d
parseGV2Token 5 d = ParseRoutine "scope=" d
parseGV2Token 6 d = ParseRoutine "" d


elementaryTokenTypes =
 ["noun", "held", "multi", "multiheld", "multiexcept",
  "multiinside", "creature", "special", "number", "topic"]


groupGV2Tokens ((0,x):rest) =
  x : groupGV2Tokens rest
groupGV2Tokens ((32,x):rest) =
  Alternatives (x : map snd xs) : groupGV2Tokens rest'
    where (xs,rest') = break (\(n,_) -> not (testBit n 4)) rest
groupGV2Tokens ((n,x):rest) = error (show n)
groupGV2Tokens [] = []


{---------------}


parseInfocomV6 = return ([],[],[])	-- FIXME


{---------------}


getGrammarTable ::
  StreamReader GrammarLine -> StreamReader [[GrammarLine]]

-- The grammar table begins with a table of pointers to entries.

getGrammarTable parser =
  do firstPtr <- getUWord
     pos      <- getPos
     remainingPtrs <- getBytes (firstPtr - pos)
     let ptrs = firstPtr : evalState (repeatUntilEmpty getUWord) remainingPtrs
     mapM (getVerbTable parser) ptrs

getVerbTable parser addr =
  do pos <- getPos
     if (pos /= addr) then contigError addr pos else do
     table <- getTable getUByte parser
     skipZeroes
     return table

contigError addr pos =
  error ("Grammar table not contiguous: next pointer is 0x" ++ showHex addr (", but expected 0x" ++ showHex pos ""))


{---------------}


skipZeroes =
  do x <- peekUByte
     when (x == 0) (getUByte >> skipZeroes)


{---------------}


{-

(parseActionCount,parseCount,parsePrepType,
 parseVerbTableBase,parseVerbDataBase,
 parseActionTableBase,parsePreactTableBase,
 parsePrepTableBase,parsePrepTableEnd) =
   findParseTableBounds parserType


findParseTableBounds Infocom6Grammar =
  let actionTableBase = wordAt (hdrObjectTableAddr-4)
      preactTableBase = wordAt (hdrObjectTableAddr-2)
      dictData  = [(b!!0 * 256 + b!!1, last b) | (_,b) <- dictEntries]  -- (parse_index,flags) pairs
      dictVerbs = filter isDictVerb dictData
      isDictVerb (parseIndex,flags) = testBit flags 0 && not (testBit flags 7) && parseIndex /= 0 && parseIndex < actionTableBase
      vbase     = minimum (map fst dictVerbs)
      vend      = maximum (map fst dictVerbs) + 8
      verbParseEntries = map (\(parseIndex,_) -> take 4 (wordsFrom parseIndex)) dictVerbs
      area2base = minimum (concatMap (drop 2) verbParseEntries)
      area2end1 = maximum (map (!! 2) verbParseEntries)
      area2end2 = maximum (map (!! 3) verbParseEntries)
      area2end  = max (area2end1 + 2 + 6 * wordAt area2end1)
                      (area2end2 + 2 + 10 * wordAt area2end2)
  in
    ((preactTableBase - actionTableBase) `div` 2, undefined, undefined,
     vbase, area2base, actionTableBase, preactTableBase, area2end, area2end)

-}


{---------------}


-- Dictionary


data DictWordTag
  = DictVerb Int | DictPrep Int
  | DictNoun | DictPlural | DictMeta
  | DictSpecial Int | DictAdj Int | DictDir Int
  | DictBytes [Int]
  | DictMaybeTruncated
  deriving (Show,Eq)


kbdInputCodes :: [Int]
dictionary    :: Array Int (String,String,[DictWordTag])

(kbdInputCodes,dictionary,numEntries,entryBaseAddr,entryLength) =
  evalFrom hdrDictionaryAddr parseDictionary


dictWordByteCount = if ver [1..3] then 4 else 6


parseDictionary = do
  kbdInputCodes <- getTable getUByte getUByte
  entryLength   <- getUByte
  numEntries    <- getUWord
  entryBaseAddr <- getPos
  entries       <- replicateM numEntries (parseDictionaryWord entryLength)
  return (kbdInputCodes, listArray (0,numEntries-1) entries,
          numEntries, entryBaseAddr, entryLength)

parseDictionaryWord entryLength = do
  encodedWord <- getBytes dictWordByteCount
  let chars = evalState getZChars encodedWord
      word  = zCharsToUnicode chars
      truncated = if maybeTruncated chars then [DictMaybeTruncated] else []
  bytes <- getBytes (entryLength - dictWordByteCount)
  let tags = truncated ++ evalState dictDataParser bytes
  return (mungeDictWord word tags, word, tags)


mungeDictWord word tags = word' ++ attr
  where word' = sfFullDictWord word
        attr | '/' `elem` word'        = ""
             | DictPlural `elem` tags  = "//p"
             | length word == 1        = "//"
             | otherwise               = ""


-- The dictionary word might be truncated even if it ends with
-- a 5 (padding value), but it's rare and clutters up the list
-- with a bunch of false positives.
maybeTruncated x = last x /= 5


dictWordAt :: Int -> String

dictWordAt addr =
  case addrToDictWordIndex addr of
    Just n  -> case dictionary!n of (word,_,_) -> word
    Nothing -> "invalidDictWord" ++ show addr


addrToDictWordIndex addr
  | addr < entryBaseAddr  = Nothing
  | otherwise =
      case (addr - entryBaseAddr) `divMod` entryLength of
        (d,0) -> if d < numEntries then Just d else Nothing
        (_,_) -> Nothing


isDictWord addr = isJust (addrToDictWordIndex addr)


{---------------}


getInfocomAdjectiveNum :: Int -> Maybe Int
getInfocomAdjectiveNum n =
  case [pos | pos <- range (bounds dictionary),
              DictAdj n' <- thd3 (dictionary!pos), n == n'] of
    []      -> Nothing
    (pos:_) -> Just (entryBaseAddr + pos * entryLength)

thd3 (_,_,z) = z


{---------------}


parseDictGV getPrepNumber =
  do flags      <- getUByte
     verbNumber <- getUByte
     prepNumber <- getPrepNumber
     return $ (if testBit flags 0 then [DictVerb verbNumber] else [])
           ++ (if testBit flags 3 then [DictPrep prepNumber] else [])
           ++ (if testBit flags 7 then [DictNoun] else [])
           ++ (if testBit flags 2 then [DictPlural] else [])
           ++ (if testBit flags 1 then [DictMeta] else [])

parseDictGV1 = parseDictGV getUByte
parseDictGV2 = parseDictGV (getUByte >> return 0)


-- FIXME: Sherlock has only two bytes per dict entry, instead
-- of three. What's up with that?

parseDictInfocom =
  do flags <- getUByte
     --
     -- Determining the contents of the next two bytes based on
     -- the flags is quite nasty, and txd/infodump isn't much
     -- help. It seems to work like this: the flag bits are
     -- checked in a certain weird order (2,3,6,7,4,5). The first
     -- one it finds set gets the value from the first byte, and
     -- the second from the second byte. Except that if the two
     -- low bits of the flag byte are nonzero, they override the
     -- default order, causing a certain bit to be checked first.
     -- If there's a third set bit, it apparently doesn't get a
     -- byte. But there's one case where bit 7 comes before 6,
     -- when the flag byte is 0xC0, and it looks like in Deadline,
     -- bits 2 and 7 are in the opposite order. I'm sure there are
     -- other nasty surprises that I didn't figure out.
     --
     let types = if flags == 0xC0 then [(0,\_->DictNoun),(1,DictVerb)] else
                 (if testBit flags 2 then [(0,DictSpecial)] else [])
              ++ (if testBit flags 3 then [(0,DictPrep)] else [])
              ++ (if testBit flags 6 then [(1,DictVerb)] else [])
              ++ (if testBit flags 7 then [(0,\_->DictNoun)] else [])
              ++ (if testBit flags 4 then [(3,DictDir)] else [])
              ++ (if testBit flags 5 then [(2,DictAdj)] else [])
         types2 = case (flags .&. 3) of
                    -- TXD defines 0 as PREP_FIRST, but it doesn't
                    -- appear to actually pull anything to the front.
                    0 -> types
                    n -> pullToFront n id types
     byte1 <- getUByte
     byte2 <- getUByte
     return $ zipWith ($) (map snd (take 2 types2)) [byte1,byte2,0]

  where pullToFront n sofar [] = error "Invalid dictionary flags byte"
        pullToFront n sofar (x@(k,_) : rest)
          | n == k    = x : sofar rest
          | otherwise = pullToFront n (sofar . (x:)) rest


-- FIXME
parseDictInfocomV6 = repeatUntilEmpty getUByte >>= \b -> return [DictBytes b]
