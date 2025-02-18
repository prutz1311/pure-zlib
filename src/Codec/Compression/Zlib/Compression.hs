{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Codec.Compression.Zlib.Compression where

import Data.Bits
import Data.Foldable (Foldable(..))
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Primitive as V
import qualified Data.Vector.Primitive.Mutable as MV
import qualified Data.Vector.Fusion.Bundle.Monadic as VM
import qualified Data.Stream.Monadic as VS
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as SBS
import qualified Data.IntMap.Strict as Map
import Data.Array (Array, accumArray, array, (!))
import Control.Monad (forM_, when, unless, zipWithM_)
import GHC.ST (ST (..))
import Data.STRef (newSTRef, modifySTRef', readSTRef, writeSTRef)
import Control.Monad.ST (stToIO)
import GHC.STRef (STRef (..))
import GHC.Word (Word8, Word16, Word32)

import Codec.Compression.Zlib.Adler32
import Codec.Compression.Zlib.Deflate
import Codec.Compression.Zlib.OutputWindow


data MatchInfo = MatchInfo
  { match_distance :: {-# UNPACK #-} !Word16
  , match_length :: {-# UNPACK #-} !Word8
  }
  deriving (Show)

encodeMatch :: Map.IntMap [Word8] -> Map.IntMap [Word8] -> MatchInfo -> [Word8]
encodeMatch lenCodes _         (MatchInfo 0 byte) = treeEncode lenCodes (fromIntegral byte)
encodeMatch lenCodes distCodes (MatchInfo dist len) =
  let (lenEntry, lenBits) = lenTableLookup len
      (distEntry, distBits) = distTableLookup dist
  in treeEncode lenCodes lenEntry ++ lenBits ++ treeEncode distCodes distEntry ++ distBits

-- Takes matches in reversed order
-- encodeMatches lenCodes distCodes matches = concatMap (encodeMatch lenCodes distCodes) (reverse matches)
encodeMatches :: Map.IntMap [Word8] -> Map.IntMap [Word8] -> [MatchInfo] -> [Word8]
encodeMatches lenCodes distCodes matches =
  foldl' (\acc m -> encodeMatch lenCodes distCodes m ++ acc) [] matches

encodeBlockFixed :: Bool -> [MatchInfo] -> [Word8]
encodeBlockFixed final matches =
  [if final then 1 else 0] ++
  toBitsPaddedNumber 2 1 ++
  encodeMatches litTable distTable matches ++
  treeEncode litTable 256

paddedLenBitsLength :: Int -> Int
paddedLenBitsLength x =
  let y = x - 3
  in (finiteBitSize y - countLeadingZeros y) - 3

paddedDistBitsLength :: Int -> Int
paddedDistBitsLength x =
  let y = x - 1
  in (finiteBitSize y - countLeadingZeros y) - 2

lenTableLookup :: Word8 -> (Int, [Word8])
lenTableLookup len =
  let (base, offset) = lenArray ! fromIntegral len
  in (base, maybe [] (toBitsPaddedNumber (paddedLenBitsLength (fromIntegral len)) . fromIntegral) offset)

distTableLookup :: Word16 -> (Int, [Word8])
distTableLookup dist =
  let (base, offset) = distArray ! fromIntegral dist
  in (base, maybe [] (toBitsPaddedNumber (paddedDistBitsLength (fromIntegral dist)) . fromIntegral) offset)

treeEncode :: Map.IntMap [Word8] -> Int -> [Word8]
treeEncode tree entry = tree Map.! entry

-- Need `accumArray` because of `258` repeating
lenArray :: Array Int (Int, Maybe Int)
lenArray = accumArray (\_ y -> y) (0, Nothing) (3, 258) $ concat
  [ zip [3..10] (zip [257..264] (repeat Nothing))
  , [(l + (c - 265) * 2 + 11, (c, Just l)) | c <- [265..268], l <- [0, 1]]
  , [(l + (c - 269) * 4 + 19, (c, Just l)) | c <- [269..272], l <- [0..3]]
  , [(l + (c - 273) * 8 + 35, (c, Just l)) | c <- [273..276], l <- [0..7]]
  , [(l + (c - 277) * 16 + 67, (c, Just l)) | c <- [277..280], l <- [0..15]]
  , [(l + (c - 281) * 32 + 131, (c, Just l)) | c <- [281..284], l <- [0..31]]
  , [(258, (285, Nothing))]
  ]

distArray :: Array Int (Int, Maybe Int)
distArray = array (1, 32768) $ concat 
  [ zip [1..4] (zip [0..3] (repeat Nothing))
  , [(d + base, (c, Just (d `mod` step)))
       | bits <- [1..13],
         let step = 2 ^ bits
             base = 2 ^ (bits + 1) + 1
             codeBase = (bits + 1) * 2,
         (d, c) <- zip [0..step-1] (repeat codeBase) ++
                   zip [step..(step*2-1)] (repeat (codeBase + 1))
    ]
  ]

boolToBit :: Bool -> Word8
boolToBit False = 0
boolToBit True = 1

toBits :: Word8 -> [Word8]
toBits x =
  let len = finiteBitSize x - countLeadingZeros x
  in map (boolToBit . testBit x) [len-1, len-2 .. 0]

toBitsPadded :: Int -> Word8 -> [Word8]
toBitsPadded len x =
  map (boolToBit . testBit x) [len-1, len-2 .. 0]

toBitsPaddedNumber :: Int -> Word8 -> [Word8]
toBitsPaddedNumber len x =
  map (boolToBit . testBit x) [0 .. len-1]

-- Takes 8-length lists
fromBits :: [Word8] -> Word8
fromBits bits = foldr (\bit_ byte -> (byte `unsafeShiftL` 1) .|. bit_) 0 bits

chunksOf :: forall a. Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = 
  let (chunk, rest) = splitAt n xs
  in chunk : chunksOf n rest

paddingZeroLength :: Int -> Int
paddingZeroLength i = (8 - i) `mod` 8

flushEmptyNonCompressedBlock :: forall s. MV.MVector s Word8 -> Int -> ST s Int
flushEmptyNonCompressedBlock slice startingBit = do
  currentBufByte <- MV.read slice 0
  let oldBits = toBitsPaddedNumber startingBit currentBufByte
      btype = [0, 0, 0]
      unpadded = oldBits ++ btype
      pad = replicate (paddingZeroLength (length unpadded)) 0
      bytes = map fromBits (chunksOf 8 (unpadded ++ pad)) ++ [0,0,255,255]
      flushedLen = length bytes
  zipWithM_ (\i b -> MV.write slice i b) [0..(flushedLen-1)] bytes
  return flushedLen

-- Starts from 0, takes a slice
flushBlock :: forall s. Bool -> MV.MVector s Word8 -> Int -> [MatchInfo] -> ST s (Int, Int)
flushBlock final outputBuf startingBit matches = do
  -- [0, 1] stands for fixed Huffman codes
  currentBufByte <- MV.read outputBuf 0
  let blockBits = encodeBlockFixed final matches
      bitsToWrite = toBitsPaddedNumber startingBit currentBufByte ++ blockBits
      bytes = map fromBits (chunksOf 8 bitsToWrite)
      (newByte, newBit) = length bitsToWrite `divMod` 8
  zipWithM_ (\i b -> MV.write outputBuf i b) [0..newByte] bytes
  return (newByte, newBit)

makeTree :: [(Int, Int, Int)] -> Map.IntMap [Word8]
makeTree treeData = Map.fromList $
  map (\(key, len, codeVal) ->
        (key, toBitsPadded len (fromIntegral codeVal))) treeData

hashFunction :: Word32 -> Word32
hashFunction val = (val * 2654435761) `unsafeShiftR` 16

read4Bytes :: V.Vector Word8 -> Word32
read4Bytes v =
  (fromIntegral (v `V.unsafeIndex` 3) `unsafeShiftL` 24) .|.
  (fromIntegral (v `V.unsafeIndex` 2) `unsafeShiftL` 16) .|.
  (fromIntegral (v `V.unsafeIndex` 1) `unsafeShiftL`  8) .|.
  (fromIntegral (v `V.unsafeIndex` 0) )

hashSubstring :: forall s. MV.MVector s Word8 -> ST s Word32
hashSubstring s = fmap (hashFunction . read4Bytes) (VG.basicUnsafeFreeze s)

insertQuick :: forall s. MV.MVector s Word8
                      -> MV.MVector s Word16
                      -> MV.MVector s Word16
                      -> Word16 
                      -> ST s Word16
insertQuick window hash prev pos = do
  let substring = MV.slice (fromIntegral pos) 4 window
  hval <- fmap fromIntegral $ hashSubstring substring
  prevHead <- MV.unsafeRead hash hval
  when (prevHead /= pos) $ do
    MV.unsafeWrite prev (fromIntegral pos) prevHead
    MV.unsafeWrite hash hval pos
  return prevHead

insert :: forall s. MV.MVector s Word8
                 -> MV.MVector s Word16
                 -> MV.MVector s Word16 
                 -> Word16 
                 -> Word16 
                 -> ST s ()
insert window hash prev pos count =
  mapM_ (\idx -> insertQuick window hash prev (pos + idx)) [0..count]

data FillResult
  = WindowFull
  | InputConsumed
  deriving (Eq, Show)

minLookahead :: Int
minLookahead = 262

maxDist :: Int
maxDist = (2::Int) ^ (15::Int) - minLookahead

wantMinMatch :: Int
wantMinMatch = 4

fillWindow :: forall s. STRef s AdlerState
                     -> MV.MVector s Word8
                     -> B.ByteString
                     -> Int
                     -> Int
                     -> Int
                     -> ST s (FillResult, (Int, Int, Int))
fillWindow checksum window inputString strstart nextIn lookahead = do
  let windowSize = MV.length window
      halfSize = windowSize `div` 2
      full = strstart >= halfSize + maxDist
      newStrstart = if full then strstart - halfSize else strstart
  -- If the window is almost full move the upper half to the lower half
  input <- copyFromByteString inputString
  when full $ do
    let left = MV.slice 0 halfSize window
        right = MV.slice halfSize halfSize window
    MV.copy left right
    MV.set right 0
  let more = windowSize - lookahead - newStrstart
      availIn = MV.length input - nextIn
      toCopy = min more availIn
      stringToCopy = B.take toCopy (B.drop nextIn inputString)
      sliceToCopy = MV.slice nextIn toCopy input
  modifySTRef' checksum (\chk -> advanceAdlerBlock chk stringToCopy)
  MV.copy (MV.slice (newStrstart + lookahead) toCopy window) sliceToCopy
  let newLookahead = lookahead + toCopy
      fillResult = if more >= availIn then InputConsumed else WindowFull
  return (fillResult, (newStrstart, newLookahead, (nextIn + toCopy)))

emitMatch :: forall s. STRef s [MatchInfo] -> MatchInfo -> ST s ()
emitMatch miStackRef mi = modifySTRef' miStackRef (mi :)

longestMatchLength :: forall s. V.Vector Word8 -> V.Vector Word8 -> ST s Int
longestMatchLength v1 v2 =
  VS.length $ VS.takeWhile id $ VS.zipWith (==) (toStream v1) (toStream v2)
  where toStream = VM.sElems . VM.fromVector

advanceWindow :: forall s. MV.MVector s Word8
                        -> MV.MVector s Word16
                        -> MV.MVector s Word16
                        -> Word16
                        -> ST s MatchInfo
advanceWindow window hash prev strstart = do
  hashHead <- insertQuick window hash prev strstart
  let dist = strstart - hashHead
  if dist > 0 && dist <= fromIntegral maxDist && hashHead /= 0
  then do
    let wsize = MV.length window
    current <- V.unsafeFreeze (MV.slice (fromIntegral strstart) (wsize - fromIntegral strstart) window)
    earlier <- V.unsafeFreeze (MV.slice (fromIntegral hashHead) (wsize - fromIntegral hashHead) window)
    len <- longestMatchLength current earlier
    if len < wantMinMatch
    then do
      literalByte <- MV.unsafeRead window (fromIntegral strstart)
      return (MatchInfo 0 literalByte)
    else do
      insert window hash prev (strstart + 1) (fromIntegral len - 1)
      return (MatchInfo dist (fromIntegral len))
  else do
    literalByte <- MV.unsafeRead window (fromIntegral strstart)
    return (MatchInfo 0 literalByte)

hasMoreInput :: FillResult -> Bool
hasMoreInput WindowFull = True
hasMoreInput InputConsumed = False

data MutableData s = MutableData
  { matchStackRef :: STRef s [MatchInfo]
  , outputBuf :: MV.MVector s Word8
  , window :: MV.MVector s Word8
  , hash :: MV.MVector s Word16
  , prev :: MV.MVector s Word16 
  , checksum :: STRef s AdlerState
  }

initializeMutableData :: forall s. ST s (MutableData s)
initializeMutableData = 
  MutableData
    <$> newSTRef []
    <*> MV.new (2048 * 1024)
    <*> MV.new (65 * 1024)
    <*> MV.new (64 * 1024)
    <*> MV.new (64 * 1024)
    <*> newSTRef initialAdlerState

clearBuffers :: forall s. MutableData s -> ST s ()
clearBuffers m = do
  writeSTRef (matchStackRef m) []
  MV.set (window m) 0
  MV.set (hash m) 0
  MV.set (prev m) 0

-- TODO: resulting BlockState is never used?
-- The loop used in `deflateFast`
dgo :: forall s. B.ByteString -> Bool -> MutableData s -> Int -> Int -> Int -> ST s ()
dgo input haveInput md@MutableData{..} strstart nextIn lookahead = do
  unless (lookahead == 0) $
    if lookahead >= wantMinMatch
    then do -- main logic
      mi@(MatchInfo dist len) <- advanceWindow window hash prev (fromIntegral strstart)
      emitMatch matchStackRef mi
      let (str, lkh) =
            if dist == 0
            then (strstart + 1, lookahead - 1)
            else (strstart + fromIntegral len, lookahead - fromIntegral len)
      (fillResult, (newStrstart, newLookahead, newNextIn)) <-
        fillWindow checksum window input str nextIn lkh
      dgo input (hasMoreInput fillResult) md newStrstart newNextIn newLookahead
    else do
      forM_ [0..(lookahead-1)] $ \i -> do -- add literal matches one by one
        let pos = fromIntegral strstart + i
        literalByte <- MV.unsafeRead window pos
        _ <- insertQuick window hash prev (fromIntegral pos)
        emitMatch matchStackRef (MatchInfo 0 literalByte)
      when haveInput $ do
        (fillResult, (newStrstart, newLookahead, newNextIn)) <-
          fillWindow checksum window input (strstart + lookahead) (nextIn + lookahead) 0
        dgo input (hasMoreInput fillResult) md newStrstart newNextIn newLookahead

copyFromByteString :: B.ByteString -> ST s (MV.MVector s Word8)
copyFromByteString bytes = V.thaw $ fromByteString $ SBS.toShort bytes

bufToList :: forall s. MV.MVector s Word8 -> ST s [Word8]
bufToList = fmap V.toList . V.unsafeFreeze

testDgo :: B.ByteString -> IO ()
testDgo inputString = do
  m <- stToIO $ do
    matchStackRef <- newSTRef []
    outputBuf <- MV.new (64 * 1024)
    window <- MV.new (65 * 1024)
    hash <- MV.new (64 * 1024)
    prev <- MV.new (64 * 1024)
    checksum <- newSTRef initialAdlerState
    (fillResult, (newStrstart, newLookahead, newNextIn)) <-
      fillWindow checksum window inputString 0 0 0
    dgo
      inputString
      (hasMoreInput fillResult)
      (MutableData matchStackRef outputBuf window hash prev checksum)
      newStrstart
      newNextIn
      newLookahead
    matches <- fmap reverse (readSTRef matchStackRef)
    return matches
  print m

writeHeader :: forall s. MV.MVector s Word8 -> ST s Int
writeHeader outputBuf = do
  MV.write outputBuf 0 0x78
  MV.write outputBuf 1 0x01
  return 2

deflateInit :: forall s. ST s (Int, MutableData s)
deflateInit = do
  m <- initializeMutableData
  len <- writeHeader (outputBuf m)
  return (len, m)

writeAdler :: AdlerState -> MV.MVector s Word8 -> ST s ()
writeAdler s slice = do
  let w :: Word32
      w = finalizeAdler s
  MV.write slice 0 (fromIntegral ((w .&. 0xff000000) `unsafeShiftR` 24))
  MV.write slice 1 (fromIntegral ((w .&. 0x00ff0000) `unsafeShiftR` 16))
  MV.write slice 2 (fromIntegral ((w .&. 0x0000ff00) `unsafeShiftR` 8))
  MV.write slice 3 (fromIntegral (w .&. 0x000000ff))

deflateFast :: forall s. Bool -> B.ByteString -> MutableData s -> Int -> ST s Int
deflateFast final inputString md@MutableData{..} outputPos = do
  (fillResult, (strstart, lookahead, nextIn)) <-
    fillWindow checksum window inputString 0 0 0
  dgo inputString (hasMoreInput fillResult) md strstart nextIn lookahead
  matches <- readSTRef matchStackRef
  (bytesWritten, nextBit) <- flushBlock final (MV.drop outputPos outputBuf) 0 matches
  clearBuffers md
  let newPos = outputPos + bytesWritten
  if final
  then do
    let adlerPos = newPos + (if nextBit == 0 then 0 else 1)
    chk <- readSTRef checksum
    writeAdler chk (MV.slice adlerPos 4 outputBuf)
    return (adlerPos + 4)
  else do
    emptyLen <- flushEmptyNonCompressedBlock (MV.drop newPos outputBuf) nextBit 
    return (newPos + emptyLen)

testDecompressMediumSingle :: B.ByteString -> ST s B.ByteString
testDecompressMediumSingle inputString = do
  (outputPos, m) <- deflateInit
  outputLen <- deflateFast True inputString m outputPos
  ovec <- V.freeze (MV.take outputLen (outputBuf m))
  return (SBS.fromShort (toByteString ovec))

testAdler :: B.ByteString -> Word32
testAdler s = finalizeAdler (advanceAdlerBlock initialAdlerState s)

testDecompressMedium :: forall s. [B.ByteString] -> ST s B.ByteString
testDecompressMedium inputStrings = do
  (outputPos, m) <- deflateInit
  let loop :: [B.ByteString] -> Int -> ST s Int
      loop []               pos = deflateFast True B.empty m pos
      loop (current : rest) pos = do
        newPos <- deflateFast False current m pos
        loop rest newPos
  outputLen <- loop inputStrings outputPos
  let finalLen = outputLen
  ovec <- V.freeze (MV.take finalLen (outputBuf m))
  return (SBS.fromShort (toByteString ovec))

chunksOfBs :: Int -> B.ByteString -> [B.ByteString]
chunksOfBs n s | B.null s  = []
               | otherwise =
  let (chunk, rest) = B.splitAt (fromIntegral n) s
  in chunk : chunksOfBs n rest

fileTest :: Int -> FilePath -> IO B.ByteString
fileTest chunkSize fn = do
  uncompr <- B.readFile fn
  let chunks = chunksOfBs chunkSize uncompr
  compr <- stToIO (testDecompressMedium chunks)
  print $ B.unpack $ B.take 100 compr
  return compr

example4test :: IO B.ByteString
example4test = fileTest 100 "/home/oleg/haskell/pure-hs-zlib/data/example4.json"

example4first1ktest :: IO B.ByteString
example4first1ktest = fileTest 100 "/home/oleg/haskell/pure-hs-zlib/data/example4.first-1024-bytes.json"

exampleRepetitive100 :: IO B.ByteString
exampleRepetitive100 = fileTest 20 "/home/oleg/haskell/pure-hs-zlib/data/repetitive100.txt"

exampleRepetitive80 :: IO B.ByteString
exampleRepetitive80 = fileTest 40 "/home/oleg/haskell/pure-hs-zlib/data/repetitive80.txt"

litTree :: [(Int, Int, Int)]
litTree = computeCodeValues
    ( [(x, 8) | x <- [0 .. 143]]
      ++ [(x, 9) | x <- [144 .. 255]]
        ++ [(x, 7) | x <- [256 .. 279]]
        ++ [(x, 8) | x <- [280 .. 287]]
    )

distTree :: [(Int, Int, Int)]
distTree = computeCodeValues [(x, 5) | x <- [0 .. 31]] 

litTable :: Map.IntMap [Word8]
litTable = makeTree litTree

distTable :: Map.IntMap [Word8]
distTable = makeTree distTree
