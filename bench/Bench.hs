{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad
import           Criterion.Main
import           Data.Bits
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Marshal
import           Foreign.Ptr
import           Foreign.Storable
import           System.Random

environment :: IO (Int, Word64, Ptr Word8)
environment = do
  randWord <- randomIO
  randInt <- pure 4 --randomRIO (0, 8)
  fPtr <- malloc
  pure (randInt, randWord, fPtr)

chopWithTemp :: Int -> Ptr Word8 -> Word64 -> IO ()
chopWithTemp 0 _ _ = pure ()
chopWithTemp !n !final !w = do
  tempBuf :: Ptr Word64 <- malloc
  poke tempBuf w
  copyBytes final (castPtr tempBuf) n

chopLoop :: Int -> Ptr Word8 -> Word64 -> IO ()
chopLoop 0 _ _ = pure ()
chopLoop n final w = do
  poke final (asWord8 w)
  chopLoop (n-1) final (shiftR w 8)

chopLogLoop :: Int -> Ptr Word8 -> Word64 -> IO ()
chopLogLoop n final w
  | n <= 0 = pure ()
  | n == 1 = poke final (asWord8 w)
  | n == 2 = poke (castPtr final) (asWord16 w)
  | n == 4 = poke (castPtr final) (asWord32 w)
  | n == 8 = poke (castPtr final) w
  | n > 4  = chopLogLoop 4 final w >>
             chopLogLoop (n-4) final (shiftR w (8*n))
  | n > 2 = chopLogLoop 2 final w >>
            chopLogLoop (n-2) final (shiftR w (8*n))

chopSpecialized :: Int -> Ptr Word8 -> Word64 -> IO ()
chopSpecialized 0 _ _     = pure ()
chopSpecialized 1 final w = poke final (asWord8 w)
chopSpecialized 2 final w = poke (castPtr final) (asWord16 w)
chopSpecialized 3 final w = do
                              chopSpecialized 2 final w
                              chopSpecialized 1 final (shiftR w 16)
chopSpecialized 4 final w = poke (castPtr final) (asWord32 w)
chopSpecialized 5 final w = do
                              chopSpecialized 4 final w
                              chopSpecialized 1 final (shiftR w 32)
chopSpecialized 6 final w = do
                              chopSpecialized 4 final w
                              chopSpecialized 2 final (shiftR w 32)
chopSpecialized 7 final w = do
                              chopSpecialized 4 final w
                              chopSpecialized 2 final (shiftR w 32)
                              chopSpecialized 1 final (shiftR w 48)
chopSpecialized 8 final w = poke (castPtr final) w
chopSpecialized _ _ _ = error "Cannot get more than 8 bytes out of a Word64"

chopFour _ final w = poke (castPtr final) (asWord32 w)

main :: IO ()
main = defaultMain [
    bgroup "Chopping Word64s" [
      env environment (\ ~(n, words, finalBuf) ->
        bench "With a temp buffer" $
          nfIO $ chopWithTemp n finalBuf words),
      env environment (\ ~(n, words, finalBuf) ->
        bench "With linear loop and shiftR" $
          whnfIO $ chopLoop n finalBuf words),
      env environment (\ ~(n, words, finalBuf) ->
        bench "With logaritmic loop and shiftR" $
          whnfIO $ chopLogLoop n finalBuf words),
      env environment (\ ~(n, words, finalBuf) ->
        bench "With unrolled loop and shiftR" $
          whnfIO $ chopSpecialized n finalBuf words),
      env environment (\ ~(n, words, finalBuf) ->
        bench "Into 4 bytes" $
          whnfIO $ chopFour n finalBuf words)
    ]
  ]


-- Names for some conversions, so we can see what's going on
asWord :: (Integral a) => a -> Word
asWord = fromIntegral

asWord8 :: (Integral a) => a -> Word8
asWord8 = fromIntegral

asWord16 :: (Integral a) => a -> Word16
asWord16 = fromIntegral

asWord32 :: (Integral a) => a -> Word32
asWord32 = fromIntegral

asWord64 :: (Integral a) => a -> Word64
asWord64 = fromIntegral
