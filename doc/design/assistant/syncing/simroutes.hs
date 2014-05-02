-- Simulation of non-flood syncing of content, across a network of nodes.

module Main where

import System.Random
import Control.Monad.Random
import Control.Monad
import Control.Applicative
import Data.Ratio
import Data.Ord
import Data.List
import qualified Data.Set as S

{-
 - Tunable values
 -}

totalFiles :: Int
totalFiles = 10

-- How likely is a given file to be wanted by any particular node?
probabilityFilesWanted :: Probability
probabilityFilesWanted = 0.10

-- How many different locations can each transfer node move between?
-- (Min, Max)
transferDestinationsRange :: (Int, Int)
transferDestinationsRange = (2, 5)

-- Controls how likely transfer nodes are to move around in a given step
-- of the simulation.
-- (They actually move slightly less because they may start to move and
-- pick the same location they are at.)
-- (Min, Max)
transferMoveFrequencyRange :: (Probability, Probability)
transferMoveFrequencyRange = (0.10, 1.00)

-- counts both immobile and transfer nodes as hops, so double Vince's
-- theoretical TTL of 3.
maxTTL :: TTL
maxTTL = TTL 6

minTTL :: TTL
minTTL = TTL 1

numImmobileNodes :: Int
numImmobileNodes = 10

numTransferNodes :: Int
numTransferNodes = 20

numSteps :: Int
numSteps = 100

-- IO code
main = putStrLn . summarize =<< evalRandIO (simulate numSteps =<< genNetwork)
-- Only pure code below :)

data Network = Network [ImmobileNode] [TransferNode]
	deriving (Show, Eq)

data ImmobileNode = ImmobileNode NodeRepo
	deriving (Show, Eq)

-- Index in the Network's list of ImmobileNodes.
type ImmobileNodeIdx = Int

data TransferNode = TransferNode
	{ currentlocation :: ImmobileNodeIdx
	, possiblelocations :: [ImmobileNodeIdx]
	, movefrequency :: Probability
	, transferrepo :: NodeRepo
	}
	deriving (Show, Eq)

data NodeRepo = NodeRepo
	{ wantFiles :: [Request]
	, haveFiles :: S.Set File
	}
	deriving (Show, Eq)

data File = File Int
	deriving (Show, Eq, Ord)

randomFile :: (RandomGen g) => Rand g File
randomFile = File <$> getRandomR (0, totalFiles)

data Request = Request File TTL
	deriving (Show)

-- compare ignoring TTL
instance Eq Request where
	(Request f1 _) == (Request f2 _) = f1 == f2

requestedFile :: Request -> File
requestedFile (Request f _) = f

requestTTL :: Request -> TTL
requestTTL (Request _ ttl) = ttl

data TTL = TTL Int
	deriving (Show, Eq, Ord)

incTTL :: TTL -> TTL
incTTL (TTL t) = TTL (t + 1)

decTTL :: TTL -> TTL
decTTL (TTL t) = TTL (t - 1)

staleTTL :: TTL -> Bool
staleTTL (TTL t) = t < 1

-- Origin of a request starts one higher than max, since the TTL
-- will decrement the first time the Request is transferred to another node.
originTTL :: TTL
originTTL = incTTL maxTTL

randomRequest :: (RandomGen g) => Rand g Request
randomRequest = Request
	<$> randomFile
	<*> pure originTTL

type Probability = Float

randomProbability :: (RandomGen g) => Rand g Probability
randomProbability = getRandomR (0, 1)

simulate :: (RandomGen g) => Int -> Network -> Rand g Network
simulate 0 net = return net
simulate c net = simulate (c - 1) =<< step net

-- Each step of the simulation, check if each TransferNode wants to move,
-- and if so:
--   1. It and its current location exchange their Requests.
--   2. And they exchange any requested files.
--   3. Move it to a new random location.
--
-- Note: This implementation does not exchange requests between two
-- TransferNodes that both arrive at the same location at the same step,
-- and then move away in the next step.
step :: (RandomGen g) => Network -> Rand g Network
step (Network immobiles transfers) = go immobiles [] transfers
  where
	go is c [] = return (Network is c)
	go is c (t:ts) = do
		r <- randomProbability
		if movefrequency t <= r
			then do
				let (is1, (currentloc:is2)) = splitAt (currentlocation t) is
				let (currentloc', t') = exchangeRequestsFiles currentloc t
				t'' <- move t'
				go (is1 ++ currentloc' : is2) (c ++ [t'']) ts
			else go is (c ++ [t]) ts

type Exchanger = ImmobileNode -> TransferNode -> (ImmobileNode, TransferNode)

exchangeRequestsFiles :: Exchanger
exchangeRequestsFiles (ImmobileNode ir) t@(TransferNode { transferrepo = tr }) =
	( ImmobileNode (go ir tr)
	, t { transferrepo = go tr ir }
	)
  where
	go r1 r2 = r1
		{ wantFiles = foldr addRequest (wantFiles r1) (wantFiles r2)
		, haveFiles = S.foldr (addFile (wantFiles r1)) (haveFiles r1) (haveFiles r2)
		}

-- Adds a file to the set, when there's a request for it.
addFile :: [Request] -> File -> S.Set File -> S.Set File
addFile rs f fs
	| any (\r -> f == requestedFile r) rs = S.insert f fs
	| otherwise = fs

-- Decrements TTL, and avoids adding request with a stale TTL, or a
-- request for an already added file with the same or a lower TTL.
addRequest :: Request -> [Request] -> [Request]
addRequest (Request f ttl) rs
	| staleTTL ttl' = rs
	| any (\r -> requestTTL r >= ttl) similar = rs
	| otherwise = r' : other
  where
	ttl' = decTTL ttl
	r' = Request f ttl'
	(other, similar) = partition (/= r') rs

move :: (RandomGen g) => TransferNode -> Rand g TransferNode
move t = do
	newloc <- randomfrom (possiblelocations t)
	return $ t { currentlocation = newloc }

genNetwork :: (RandomGen g) => Rand g Network
genNetwork = do
	immobiles <- sequence (replicate numImmobileNodes mkImmobile)
	transfers <- sequence (replicate numTransferNodes (mkTransfer immobiles))
	return $ Network immobiles transfers

mkImmobile :: (RandomGen g) => Rand g ImmobileNode
mkImmobile = ImmobileNode <$> genrepo
  where
	genrepo = NodeRepo
		-- The files this node wants.
		-- Currently assumes each file is equally popular.
		<$> sequence (replicate (truncate (fromIntegral totalFiles * probabilityFilesWanted)) randomRequest)
		-- The files this node already has.
		--
		-- We'll assume equal production, so split the total
		-- number of files amoung the immobile nodes.
		-- (This will produce some duplication of files
		--  (consider birthday paradox), and some missing files.)
		--
		-- TODO: Some immobile nodes are internet connected,
		-- and these should all share their files automatically)
		-- (Also when running the sim.)
		<*> (S.fromList <$> sequence (replicate (totalFiles `div` numImmobileNodes) randomFile))

mkTransfer :: (RandomGen g) => [ImmobileNode] -> Rand g TransferNode
mkTransfer immobiles = do
  	-- Transfer nodes are given random routes. May be simplistic.
	-- Also, some immobile nodes will not be serviced by any transfer nodes.
	numpossiblelocs <- getRandomR transferDestinationsRange
	possiblelocs <- sequence (replicate numpossiblelocs (randomfrom indexes))
	currentloc <- randomfrom possiblelocs
	movefreq <- getRandomR transferMoveFrequencyRange
	-- transfer nodes start out with no files or requests in their repo
	let repo = (NodeRepo [] S.empty)
	return $ TransferNode currentloc possiblelocs movefreq repo
  where
	indexes = [0..length immobiles - 1]

randomfrom :: (RandomGen g) => [a] -> Rand g a
randomfrom l = do
	i <- getRandomR (1, length l)
	return $ l !! (i - 1)

summarize :: Network -> String
summarize (Network is _ts) = unlines $ map (\(d, s) -> d ++ ": " ++ s)
	[ ("Total wanted files",
		show (sum (overis (length . findoriginreqs . wantFiles . repo))))
	, ("Wanted files that were not transferred to requesting node",
		show (sum (overis (S.size . findunsatisfied . repo))))
	--, ("List of files not transferred", show unsatisfied)
	, ("Immobile nodes at end", show is)
	]
  where
  	findoriginreqs = filter (\r -> requestTTL r == originTTL)
	findunsatisfied r = 
		let wantedfs = S.fromList $ map requestedFile (findoriginreqs (wantFiles r))
		in S.difference wantedfs (haveFiles r)
	repo (ImmobileNode r) = r
	overis f = map f is
