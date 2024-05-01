{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Codec.Compression.Zlib.Compression where

import Data.Bits
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Primitive as V
import qualified Data.Vector.Primitive.Mutable as MV
import qualified Data.Vector.Fusion.Bundle.Monadic as VM
import qualified Data.Stream.Monadic as VS
import qualified Data.ByteString as B
import qualified Data.IntMap.Strict as Map
import Data.Array (Array, accumArray, array, (!))
import Control.Monad (forM_, when)
import GHC.ST (ST (..))
import Control.Monad.ST (stToIO)
import GHC.STRef (STRef (..))
import GHC.Word (Word8, Word16, Word32)

data MatchInfo = MatchInfo
  { match_distance :: {-# UNPACK #-} !Word16
  , match_length :: {-# UNPACK #-} !Word8
  }
  deriving (Show)

encodeMatch :: Map.IntMap [Word8] -> Map.IntMap [Word8] -> MatchInfo -> [Word8]
encodeMatch lenCodes distCodes (MatchInfo 0 byte) = treeEncode lenCodes (fromIntegral byte)
encodeMatch lenCodes distCodes (MatchInfo dist len) =
  let (lenEntry, lenBits) = lenTableLookup len
      (distEntry, distBits) = distTableLookup dist
  in treeEncode lenCodes lenEntry ++ lenBits ++ treeEncode distCodes distEntry ++ distBits

lenTableLookup :: Word8 -> (Int, [Word8])
lenTableLookup = error "TODO: "

distTableLookup :: Word16 -> (Int, [Word8])
distTableLookup = error "TODO: "

treeEncode :: Map.IntMap [Word8] -> Int -> [Word8]
treeEncode tree entry = tree Map.! entry

-- Need `accumArray` because of `258` repeating
lenArray :: Array Int (Int, Int)
lenArray = accumArray (\_ y -> y) (0, 0) (3, 258) $ concat
  [ zip [3..10] (zip [257..264] (repeat 0))
  , [(l + (c - 265) * 2 + 11, (c, l)) | c <- [265..268], l <- [0, 1]]
  , [(l + (c - 269) * 4 + 19, (c, l)) | c <- [269..272], l <- [0..3]]
  , [(l + (c - 273) * 8 + 35, (c, l)) | c <- [273..276], l <- [0..7]]
  , [(l + (c - 277) * 16 + 67, (c, l)) | c <- [277..280], l <- [0..15]]
  , [(l + (c - 281) * 32 + 131, (c, l)) | c <- [281..284], l <- [0..31]]
  , [(258, (285, 0))]
  ]

distArray :: Array Int (Int, Int)
distArray = array (1, 32768) $ concat 
  [ zip [1..4] (zip [0..3] (repeat 0))
  , [(d + base, (c, d `mod` step))
       | bits <- [1..13],
         let step = 2 ^ bits
             base = 2 ^ (bits + 1) + 1
             codeBase = (bits + 1) * 2,
         (d, c) <- zip [0..step-1] (repeat codeBase) ++
                   zip [step..(step*2-1)] (repeat (codeBase + 1))
    ]
  ] -- TODO: pairs? (extract `d` too)

toBits :: Word8 -> [Word8]
toBits x =
  let len = finiteBitSize x - countLeadingZeros x
      boolToBit False = 0
      boolToBit True = 1
  in map (boolToBit . testBit x) [len-1, len-2 .. 0]

    -- (0, 29)
    -- [ (0, return 1)
    -- , (1, return 2)
    -- , (2, return 3)
    -- , (3, return 4)
    -- , (4, (+ 5) `fmap` nextBits 1)
    -- , (5, (+ 7) `fmap` nextBits 1)
    -- , (6, (+ 9) `fmap` nextBits 2)
    -- , (7, (+ 13) `fmap` nextBits 2)
    -- , (8, (+ 17) `fmap` nextBits 3)
    -- , (9, (+ 25) `fmap` nextBits 3)
    -- , (10, (+ 33) `fmap` nextBits 4)
    -- , (11, (+ 49) `fmap` nextBits 4)
    -- , (12, (+ 65) `fmap` nextBits 5)
    -- , (13, (+ 97) `fmap` nextBits 5)
    -- , (14, (+ 129) `fmap` nextBits 6)
    -- , (15, (+ 193) `fmap` nextBits 6)
    -- , (16, (+ 257) `fmap` nextBits 7)
    -- , (17, (+ 385) `fmap` nextBits 7)
    -- , (18, (+ 513) `fmap` nextBits 8)
    -- , (19, (+ 769) `fmap` nextBits 8)
    -- , (20, (+ 1025) `fmap` nextBits 9)
    -- , (21, (+ 1537) `fmap` nextBits 9)
    -- , (22, (+ 2049) `fmap` nextBits 10)
    -- , (23, (+ 3073) `fmap` nextBits 10)
    -- , (24, (+ 4097) `fmap` nextBits 11)
    -- , (25, (+ 6145) `fmap` nextBits 11)
    -- , (26, (+ 8193) `fmap` nextBits 12)
    -- , (27, (+ 12289) `fmap` nextBits 12)
    -- , (28, (+ 16385) `fmap` nextBits 13)
    -- , (29, (+ 24577) `fmap` nextBits 13)
    -- ]


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

insertQuick :: forall s. MV.MVector s Word8 -> MV.MVector s Word16 -> MV.MVector s Word16 -> Word16 -> ST s Word16
insertQuick window hash prev pos = do
  let substring = MV.slice (fromIntegral pos) 4 window
  h <- fmap fromIntegral $ hashSubstring substring
  prevHead <- MV.unsafeRead hash h
  when (prevHead /= pos) $ do
    MV.unsafeWrite prev (fromIntegral pos) prevHead
    MV.unsafeWrite hash h pos -- h TODO
  return prevHead

insert :: forall s. MV.MVector s Word8 -> MV.MVector s Word16 -> MV.MVector s Word16 -> Word16 -> Word16 -> ST s ()
insert window hash prev pos count = mapM_ (\idx -> insertQuick window hash prev (pos + idx)) [0..count]

inspectArray :: forall a. MV.Prim a => MV.MVector (MV.RealWorld) a -> IO [a]
inspectArray v = fmap V.toList $ stToIO (V.freeze v)

inspectArrayNonzero :: forall a. (Eq a, Num a, MV.Prim a) => MV.MVector (MV.RealWorld) a -> IO [(Int, a)]
inspectArrayNonzero v = fmap (\arr -> filter ((/= 0) . snd) (zip [0..] arr)) (inspectArray v)

createArrays :: forall s. B.ByteString -> ST s (MV.MVector s Word8, MV.MVector s Word16, MV.MVector s Word16)
createArrays input = do
  let wMax :: Int
      wMax = 2^15
      wLen = min (B.length input) (2^15)
  window <- MV.replicate wMax 0
  mapM_ (\i -> MV.write window i (B.index input i)) [0..(wLen-1)]
  hash <- MV.replicate wMax 0
  prev <- MV.replicate wMax 0
  return (window, hash, prev)

data DeflateState = DeflateState
  { _dst_strstart :: !Int
  , _dst_lookahead :: !Int
  , _dst_symnext :: !Int
  } deriving (Show)

data FillResult
  = WindowFull !Int !Int !Int
  | InputConsumed !Int !Int !Int
  deriving (Eq, Show)

minLookahead :: Int
minLookahead = 262

maxDist :: Int
maxDist = (2::Int) ^ (15::Int) - minLookahead

wantMinMatch :: Int
wantMinMatch = 4

-- moveToLowerHalf :: forall s. MV.MVector s Word8 -> Int -> ST s ()
-- moveToLowerHalf window strstart = do
--   let left = MV.slice 0 halfSize window
--       right = MV.slice halfSize halfSize window
--   MV.copy left right
--   MV.set right 0

fillWindow :: forall s. MV.MVector s Word8 -> MV.MVector s Word8 -> Int -> Int -> Int -> ST s FillResult
fillWindow window input strstart nextIn lookahead = do
  let windowSize = MV.length window
      halfSize = windowSize `div` 2
      full = strstart >= halfSize + maxDist
      newStrstart = if full then strstart - halfSize else strstart
  -- If the window is almost full move the upper half to the lower half
  when full $ do
    let left = MV.slice 0 halfSize window
        right = MV.slice halfSize halfSize window
    MV.copy left right
    MV.set right 0
  let more = windowSize - lookahead - newStrstart
      availIn = MV.length input - nextIn
      toCopy = min more availIn
  -- TODO: add checksum updating
  MV.copy (MV.slice (newStrstart + lookahead) toCopy window) (MV.slice nextIn toCopy input)
  let newLookahead = lookahead + toCopy
  return $ if more >= availIn then InputConsumed newStrstart newLookahead (nextIn + toCopy) else WindowFull newStrstart newLookahead (nextIn + toCopy)

data BlockState
  = NeedMore
  | BlockDone
  | FinishStarted
  | FinishDone
  deriving (Eq, Show)

deflateMedium :: forall s. STRef s DeflateState -> ST s BlockState
deflateMedium dstate = do
  return FinishDone

emitMatch :: forall s. MV.MVector s Word8 -> MatchInfo -> Int -> ST s Int
emitMatch matchBuffer (MatchInfo dist len) oldSymnext = do
  MV.write matchBuffer oldSymnext (fromIntegral (dist .&. 0xff))
  MV.write matchBuffer (oldSymnext + 1) (fromIntegral (dist `unsafeShiftR` 8))
  MV.write matchBuffer (oldSymnext + 2) len
  return (oldSymnext + 3)

longestMatchLength :: forall s. V.Vector Word8 -> V.Vector Word8 -> ST s Int
longestMatchLength v1 v2 = VS.length $ VS.takeWhile id $ VS.zipWith (==) (toStream v1) (toStream v2)
  where toStream = VM.sElems . VM.fromVector

advanceWindow :: forall s. MV.MVector s Word8 -> MV.MVector s Word16 -> MV.MVector s Word16 -> Word16 -> ST s MatchInfo
advanceWindow window hash prev strstart = do
  hashHead <- insertQuick window hash prev strstart
  let dist = strstart - hashHead
  -- TODO: MAX_DIST(s)
  if dist > 0 && hashHead /= 0
  then do
    let wsize = MV.length window
    current <- V.freeze (MV.slice (fromIntegral strstart) (wsize - fromIntegral strstart) window)
    earlier <- V.freeze (MV.slice (fromIntegral hashHead) (wsize - fromIntegral hashHead) window)
    len <- longestMatchLength current earlier
    insert window hash prev (strstart + 1) (fromIntegral len - 1)
    return (MatchInfo dist (fromIntegral len))
  else do
    literalByte <- MV.unsafeRead window (fromIntegral strstart)
    return (MatchInfo 0 literalByte)

dgo :: forall s. Bool -> MV.MVector s Word8 -> Int -> MV.MVector s Word8 -> MV.MVector s Word16 -> MV.MVector s Word16 -> MV.MVector s Word8 -> Int -> Int -> Int -> ST s BlockState
dgo haveInput matchBuffer symnext window hash prev input strstart nextIn lookahead = do
  if lookahead == 0
  then return BlockDone
  else if lookahead >= wantMinMatch
       then do -- main logic
         mi@(MatchInfo dist len) <- advanceWindow window hash prev (fromIntegral strstart)
         _ <- emitMatch matchBuffer mi symnext
         let (str, lkh) = 
               if dist == 0
               then (strstart + 1, lookahead - 1)
               else (strstart + fromIntegral len, lookahead - fromIntegral len)
         fillResult <- fillWindow window input str nextIn lkh
         case fillResult of
           WindowFull newStrstart newLookahead newNextIn -> do
             dgo True matchBuffer (symnext + 3) window hash prev input newStrstart newNextIn newLookahead
           InputConsumed newStrstart newLookahead newNextIn -> do
             dgo False matchBuffer (symnext + 3) window hash prev input newStrstart newNextIn newLookahead
       else do
         forM_ [0..(lookahead-1)] $ \i -> do -- add literal matches one by one
           let pos = fromIntegral strstart + i
           literalByte <- MV.unsafeRead window pos
           _ <- insertQuick window hash prev (fromIntegral pos)
           _ <- emitMatch matchBuffer (MatchInfo 0 literalByte) symnext
           return ()
         if haveInput
         then do
           fillResult <- fillWindow window input (strstart + lookahead) (nextIn + lookahead) 0
           case fillResult of
             WindowFull newStrstart newLookahead newNextIn -> do
               dgo True matchBuffer (symnext + lookahead * 3) window hash prev input newStrstart newNextIn newLookahead
             InputConsumed newStrstart newLookahead newNextIn -> do
               dgo False matchBuffer (symnext + lookahead * 3) window hash prev input newStrstart newNextIn newLookahead
         else return BlockDone
