import Debug.Trace

import Intcode

import qualified Data.Text.IO as TIO

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Data.List
-- import Data.Function (on)
import Control.Lens
import Control.Monad.RWS.Strict


data EncapsulatedMacine = EncapsulatedMacine 
    { _machine :: Machine
    , _executionState :: ExecutionState
    , _currentInput :: [Integer]
    , _machineOutput :: [Integer]
    } deriving (Show, Eq)
makeLenses ''EncapsulatedMacine    

type Network = M.Map Integer EncapsulatedMacine

data Packet = Packet 
    { _destination :: Integer
    , _packetX :: Integer
    , _packetY :: Integer
    } deriving (Show, Eq)
makeLenses ''Packet    


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent23.txt"
        let mem = parseMachineMemory text
        print $ part1 mem
        -- print $ part2 mem


part1 mem = runNetworkUntil255 net
    where   net = buildNetwork mem

runNetworkUntil255 :: Network -> Packet
runNetworkUntil255 net0
    | not $ null goalPackets = head packets
    | otherwise = runNetworkUntil255 net3
    where   net1 = runNetworkStep net0
            (net2, packets) = extractPackets net1
            net3 = enqueuePackets net2 packets
            goalPackets = filter (packet255) packets


packet255 :: Packet -> Bool
packet255 packet = (packet ^. destination) == 255


buildNetwork :: [Integer] -> Network
buildNetwork mem = M.fromList $ map (\i -> (i, encapsulate mem i)) [0..49]


encapsulate :: [Integer] -> Integer -> EncapsulatedMacine
encapsulate mem input = EncapsulatedMacine 
    { _machine = makeMachine mem
    , _executionState = Runnable
    , _machineOutput = []
    , _currentInput = [input]
    }




-- pipelineTrace :: Pipeline -> String
-- pipelineTrace pipeline = show $ M.toList $ M.map emTrace pipeline

-- emTrace e = intercalate " ; " terms
--     where terms = [ show $ _executionState e
--                   , "in"
--                   , show $ _currentInput e 
--                   , "out"
--                   , show $ _machineOutput e
--                   ]

runNetworkStep :: Network -> Network
runNetworkStep net = M.map runEncapsulatedMachine net

extractPackets :: Network -> (Network, [Packet])
extractPackets net = (net', packets)
    where   packets = concat $ M.elems $ M.map extractPacket net
            net' = M.map stripPacket net

extractPacket :: EncapsulatedMacine -> [Packet]
extractPacket e = if length output >= 3
                  then [Packet { _destination = fromIntegral $ output!!0
                               , _packetX = output!!1
                               , _packetY = output!!2} ]
                  else []
    where   output = (e ^. machineOutput)

stripPacket :: EncapsulatedMacine -> EncapsulatedMacine
stripPacket e = if length (e ^. machineOutput) >= 3
                then e & machineOutput %~ (drop 3)
                else e

enqueuePackets :: Network -> [Packet] -> Network
enqueuePackets net packets = foldl' enqueuePacket net packets

enqueuePacket :: Network -> Packet -> Network
enqueuePacket net packet
    | d `M.member` net = M.insert d e' net
    | otherwise = net
    where   d = packet ^. destination
            e = net!d
            e' = e & currentInput %~ (++ [packet ^. packetX, packet ^. packetY])


runEncapsulatedMachine :: EncapsulatedMacine -> EncapsulatedMacine
runEncapsulatedMachine e = e & machine .~ m''
                             & executionState .~ halted'
                             & currentInput .~ input'
                             & machineOutput %~ ( ++ output' )
    where   (halted, m', output) = runRWS runNetworkMachineStep (e ^. currentInput) (e ^. machine)
            input' = if halted == Blocked
                     then (e ^. currentInput) ++ [-1]
                     else e ^. currentInput
            (halted', m'', output') = if halted == Blocked
                                      then runRWS runNetworkMachineStep input' (e ^. machine)
                                      else (halted, m', output)


runNetworkMachineStep :: ProgrammedMachine ExecutionState
runNetworkMachineStep = do 
    mem <- gets _memory
    ip <- gets _ip
    input <- ask
    iIndex <- gets _inputIndex
    let acutalInputLength = length input
    let requiredInputLength = iIndex + 1
    if (mem!ip == 99)
    then return Terminated
    else if (mem!ip == 3 && requiredInputLength > acutalInputLength)
         then return Blocked
         else do 
                runStep
                return Runnable
