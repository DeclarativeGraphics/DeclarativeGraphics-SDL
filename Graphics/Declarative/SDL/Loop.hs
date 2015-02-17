module Graphics.Declarative.SDL.Loop where

import Control.Concurrent (threadDelay)
import qualified Graphics.UI.SDL as SDL

--import Text.Printf

ioLoop :: (s -> IO (Bool, s)) -> s -> IO s
ioLoop loopFunc initialState = do
  (shouldContinue, nextIn) <- loopFunc initialState
  if shouldContinue
    then ioLoop loopFunc nextIn
    else return nextIn

scheduledLoopStateless :: Int -> IO Bool -> IO ()
scheduledLoopStateless runsPerSec action = scheduledLoop runsPerSec igAction ()
  where igAction () = fmap (\shouldContinue -> (shouldContinue, ())) action

scheduledLoop :: Int -> s -> (s -> IO (Bool, s)) -> IO s
scheduledLoop runsPerSec initialState loopFunc = do
  initTimeStamp <- getMicrosecondTime
  (_, s) <- ioLoop (loopF initTimeStamp) (0, initialState)
  return s
  where
    targetStepTime = 1000 `div` runsPerSec
    --loopF :: Int -> (Int, s) -> IO (Bool, (Int, s))
    loopF initTimeStamp (prevTargetTime, state) = do
      --beforeStamp <- getMicrosecondTime
      (shouldContinue, newState) <- loopFunc state
      timeStamp <- getMicrosecondTime
      let actualTime = timeStamp - initTimeStamp
      let targetTime = prevTargetTime + targetStepTime
      let sleepTime = targetTime - actualTime
      --let simTime = timeStamp - beforeStamp
      --printf "target: %5d, simTime: %3d, real: %5d, sleep: %3d\n" (div targetTime 1000) (div simTime 1000) (div actualTime 1000) (div sleepTime 1000)
      threadDelay sleepTime
      return (shouldContinue, (targetTime, newState))

getMicrosecondTime :: IO Int
getMicrosecondTime = fmap fromIntegral SDL.getTicks
