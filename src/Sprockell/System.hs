{-# LANGUAGE RecordWildCards #-}

module Sprockell.System where

import Sprockell.BasicFunctions
import Sprockell.HardwareTypes
import Sprockell.Sprockell

numberIOaddr,charIOaddr :: MemAddr
numberIOaddr = 0x10000
charIOaddr   = 0x10001

numberIO, charIO :: AddrImmDI
numberIO = DirAddr numberIOaddr
charIO   = DirAddr charIOaddr

-- ===================================================================================
shMem :: SharedMem
         -> (SprID,Request)
         -> (SharedMem, (SprID,Reply))

shMem sharedMem (i,req) = (sharedMem', (i,reply))
        where
          (reply, sharedMem')   = case req of
                NoRequest                          -> ( Nothing            , sharedMem )
                ReadReq a                          -> ( Just (sharedMem!a) , sharedMem )
                WriteReq v a                       -> ( Nothing            , sharedMem <~ (a,v))
                TestReq a     | sharedMem!a == 0   -> ( Just 1             , sharedMem <~ (a,1))
                              | otherwise          -> ( Just 0             , sharedMem )

reqAddr :: Request -> Maybe MemAddr
reqAddr req = case req of
    NoRequest    -> Nothing
    ReadReq a    -> Just a
    WriteReq _ a -> Just a
    TestReq a    -> Just a

isNumberIOreq, isCharIOreq :: Request -> Bool
isNumberIOreq req = reqAddr req == Just numberIOaddr
isCharIOreq   req = reqAddr req == Just charIOaddr


updateFifo :: RequestFifo
              -> IndRequests
              -> (RequestFifo, (SprID,Request))
updateFifo requestFifo chRequests = (requestFifo', req)
        where
          req  | not $ null requestFifo = head requestFifo
               | otherwise              = (0, NoRequest)
          requestFifo' = drop 1 requestFifo ++ filter ((/=NoRequest).snd) chRequests

-- ===================================================================================
transferA :: (RequestChannels, ReplyChannels)
                -> (ParRequests)
                -> ((RequestChannels), (ParReplies, IndRequests))

transferA (requestChnls,replyChnls) (sprRequests) = ( (requestChnls'), (outReplies,outRequests) )
        where
          -- ->->->->
          outRequests   = zip [0..] $ map head requestChnls                                             -- <<== TODO: abstract away from softare/hardware
          requestChnls' = zipWith (<<+) requestChnls sprRequests

          -- <-<-<-<-
          n             = length replyChnls                                                             -- <<== TODO: abstraction difficult:
          --inReplies     = replicate n Nothing <~ (i,shMemReply)                                         --              no parameter n in CLaSH
          outReplies    = map head replyChnls
          --replyChnls'   = zipWith (<<+) replyChnls inReplies

transferB :: ReplyChannels
                -> (SprID, Reply)
                -> ReplyChannels
transferB replyChnls (i,shMemReply) = replyChnls'
        where
          -- <-<-<-<-
          n             = length replyChnls                                                             -- <<== TODO: abstraction difficult:
          inReplies     = replicate n Nothing <~ (i,shMemReply)                                         --              no parameter n in CLaSH
          replyChnls'   = zipWith (<<+) replyChnls inReplies

-- ===================================================================================
system :: Int -> [InstructionMem] -> SystemState -> t -> SystemState

system nrOfSprs instrss systemState _ = systemState'
        where
          SystemState{..} = systemState

          -- Sprockells
          (sprStates',sprRequests)                              = unzip $ sprockell $> instrss |$| sprStates |$| chReplies

          -- Communication
          ((requestChnls'), (chReplies,chRequests)) = transferA (requestChnls,replyChnls) (sprRequests)
          replyChnls' = transferB replyChnls (i,shMemReply)

          (requestFifo',request) = updateFifo requestFifo chRequests
          -- Shared Memory
          (sharedMem', (i,shMemReply))           = shMem sharedMem request

          systemState' = SystemState
                { sprStates     = sprStates'
                , requestChnls  = requestChnls'
                , replyChnls    = replyChnls'
                , requestFifo   = requestFifo'
                , sharedMem     = sharedMem'
                }
