import Debug.Trace

import Intcode

import qualified Data.Text.IO as TIO

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Data.List
-- import Data.Foldable
-- import Data.Function (on)
import Control.Lens
import Control.Monad.RWS.Strict


data EncapsulatedMacine = EncapsulatedMacine 
    { _machine :: Machine
    , _executionState :: ExecutionState
    , _currentInput :: [Integer]
    , _machineOutput :: [Integer]
    } deriving (Eq)
makeLenses ''EncapsulatedMacine    

instance Show EncapsulatedMacine where 
    show e = "EncapsulatedMacine {_machine = <>, _executionState = " ++ show (e ^. executionState) ++ ", _currentInput = " ++ show (e ^. currentInput) ++ ", _machineOutput = " ++ show (e ^. machineOutput) ++ "}"

type Network = M.Map Integer EncapsulatedMacine

data Packet = Packet 
    { _destination :: Integer
    , _packetX :: Integer
    , _packetY :: Integer
    } deriving (Show, Eq, Ord)
makeLenses ''Packet    

data NatNetwork = NatNetwork
    { _natNetwork :: Network
    , _natPacket :: Packet
    , _natPreviousY :: Integer
    } deriving (Show, Eq)
makeLenses ''NatNetwork


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent23.txt"
        let mem = parseMachineMemory text
        print $ part1 mem
        print $ part2 mem


part1 mem = (runNetworkUntil255 net) ^. packetY
    where   net = buildNetwork mem


part2 mem = runNetworkUntilTermination natNet
    where natNet = buildNatNetwork mem


runNetworkUntil255 :: Network -> Packet
runNetworkUntil255 net0
    | not $ null goalPackets = head goalPackets
    | otherwise = runNetworkUntil255 net3
    where   net1 = runNetworkStep net0
            (net2, packets) = extractPackets net1
            net3 = enqueuePackets net2 packets
            goalPackets = filter isNatPacket packets

runNetworkUntilTermination :: NatNetwork -> Integer
-- runNetworkUntilTermination natNet | trace ("Nat: " ++ show (natNet ^. natPacket) ++ " last = " ++ show (natNet ^. natPreviousY)) False = undefined
runNetworkUntilTermination natNet
    | part2Termination natNet1 = natNet1 ^. natPacket . packetY
    | otherwise = runNetworkUntilTermination natNet2
    where natNet1 = runNetworkUntilIdle natNet
          np = (natNet1 ^. natPacket) & destination .~ 0
          net = natNet1 ^. natNetwork
          net2 = enqueuePacket net np
          natNet2 = natNet1 & natNetwork .~ net2
                            & natPreviousY .~ (np ^. packetY)
                            & natPacket .~ emptyPacket

part2Termination :: NatNetwork -> Bool
-- part2Termination natNet | trace ("Term. this: " ++ (show (natNet ^. natPacket)) ++ " prev: " ++ (show (natNet ^. natPreviousY))) False = undefined
part2Termination natNet = thisY == prevY
    where thisY = natNet ^. natPacket . packetY
          prevY = natNet ^. natPreviousY


runNetworkUntilIdle :: NatNetwork -> NatNetwork
runNetworkUntilIdle natNet
    | isIdle net0 = natNet
    | otherwise = runNetworkUntilIdle natNet'
    where   net0 = natNet ^. natNetwork
            net1 = runNetworkStep net0
            (net2, packets) = extractPackets net1
            net3 = enqueuePackets net2 packets
            natPackets = filter isNatPacket packets
            np = if null natPackets 
                 then natNet ^. natPacket
                 else head natPackets
            natNet' = natNet & natNetwork .~ net3
                             & natPacket .~ np


emptyPacket :: Packet
emptyPacket = Packet {_destination = 0, _packetX = 0, _packetY = 0}

isNatPacket :: Packet -> Bool
isNatPacket packet = (packet ^. destination) == 255

isIdle :: Network -> Bool
isIdle net = inputBlocked && noOutput
    where inputBlocked = all (\e -> (last $ e ^. currentInput) == -1 && (last $ init $ e ^. currentInput) == -1) $ M.elems net
          noOutput = all (\e -> null $ e ^. machineOutput) $ M.elems net


buildNatNetwork :: [Integer] -> NatNetwork
buildNatNetwork mem = NatNetwork 
    { _natNetwork = buildNetwork mem
    , _natPacket = emptyPacket
    , _natPreviousY = -1
    }

buildNetwork :: [Integer] -> Network
buildNetwork mem = M.fromList $ map (\i -> (i, encapsulate mem i)) [0..49]


encapsulate :: [Integer] -> Integer -> EncapsulatedMacine
encapsulate mem input = EncapsulatedMacine 
    { _machine = makeMachine mem
    , _executionState = Runnable
    , _machineOutput = []
    , _currentInput = [input]
    }



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
-- enqueuePacket _ packet | trace ("Enqueue " ++ show packet) False = undefined
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
