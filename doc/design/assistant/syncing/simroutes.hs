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
import qualified Data.Map.Strict as M

{-
 - Tunable values
 -}

totalFiles :: Int
totalFiles = 100

-- How likely is a given file to be wanted by any particular node?
probabilityFilesWanted :: Probability
probabilityFilesWanted = 0.10

-- How many different locations can each transfer node move between?
-- (Min, Max)
transferDestinationsRange :: (Int, Int)
transferDestinationsRange = (2, 3)

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
--main = putStrLn . summarize =<< evalRandIO (simulate numSteps =<< genNetwork)
main = do
	initialnetwork <- evalRandIO mocambos
	putStrLn . summarize initialnetwork
		=<< evalRandIO (simulate numSteps initialnetwork)
-- Only pure code below :)

data Network = Network (M.Map NodeName ImmobileNode) [TransferNode]
	deriving (Show, Eq)

data ImmobileNode = ImmobileNode NodeRepo
	deriving (Show, Eq)

type NodeName = String

type Route = [NodeName]

data TransferNode = TransferNode
	{ currentlocation :: NodeName
	, possiblelocations :: [NodeName]
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
			then case M.lookup (currentlocation t) is of
				Nothing -> go is (c ++ [t]) ts
				Just currentloc -> do
					let (currentloc', t') = exchangeRequestsFiles currentloc t
					t'' <- move t'
					go (M.insert (currentlocation t) currentloc' is) (c ++ [t'']) ts
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
	l <- sequence (replicate numImmobileNodes mkImmobile)
	let immobiles = M.fromList (zip (map show [1..]) l)
	transfers <- sequence (replicate numTransferNodes (mkTransfer $ M.keys immobiles))
	return $ Network immobiles transfers

mkImmobile :: (RandomGen g) => Rand g ImmobileNode
mkImmobile = ImmobileNode <$> mkImmobileRepo

mkImmobileRepo :: (RandomGen g) => Rand g NodeRepo
mkImmobileRepo = NodeRepo
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

mkTransfer :: (RandomGen g) => [NodeName] -> Rand g TransferNode
mkTransfer immobiles = do
  	-- Transfer nodes are given random routes. May be simplistic.
	-- Also, some immobile nodes will not be serviced by any transfer nodes.
	numpossiblelocs <- getRandomR transferDestinationsRange
	possiblelocs <- sequence (replicate numpossiblelocs (randomfrom immobiles))
	mkTransferBetween possiblelocs

mkTransferBetween :: (RandomGen g) => [NodeName] -> Rand g TransferNode
mkTransferBetween possiblelocs = do
	currentloc <- randomfrom possiblelocs
	movefreq <- getRandomR transferMoveFrequencyRange
	-- transfer nodes start out with no files or requests in their repo
	let repo = (NodeRepo [] S.empty)
	return $ TransferNode currentloc possiblelocs movefreq repo

randomfrom :: (RandomGen g) => [a] -> Rand g a
randomfrom l = do
	i <- getRandomR (1, length l)
	return $ l !! (i - 1)

summarize :: Network -> Network -> String
summarize _initial@(Network origis _) _final@(Network is _ts) = format
	[ ("Total wanted files",
		show (sum (overis (length . findoriginreqs . wantFiles . repo))))
	, ("Wanted files that were not transferred to requesting node",
		show (sum (overis (S.size . findunsatisfied . repo))))
	, ("Nodes that failed to get files",
		show (map withinitiallocs $ filter (not . S.null . snd)
			(M.toList $ M.map (findunsatisfied . repo) is)))
	--, ("Immobile nodes at end", show is)
	]
  where
  	findoriginreqs = filter (\r -> requestTTL r == originTTL)
	findunsatisfied r = 
		let wantedfs = S.fromList $ map requestedFile (findoriginreqs (wantFiles r))
		in S.difference wantedfs (haveFiles r)
	repo (ImmobileNode r) = r
	overis f = map f $ M.elems is
	format = unlines . map (\(d, s) -> d ++ ": " ++ s)

	withinitiallocs (name, missingfiles) = (name, S.map addinitialloc missingfiles)
	addinitialloc f = (f, M.lookup f initiallocs)

	initiallocs = M.fromList $ 
		concatMap (\(k, v) -> map (\f -> (f, k)) (S.toList $ haveFiles $ repo v)) $
			M.toList origis

mocambos :: (RandomGen g) => Rand g Network
mocambos = do
	major <- mapM (immobilenamed . fst) communities
	minor <- mapM immobilenamed (concatMap snd communities)
	majortransfer <- mapM mkTransferBetween majorroutes
	minortransfer <- mapM mkTransferBetween (concatMap minorroutes communities)
	return $ Network
		(M.fromList (major++minor))
		(majortransfer ++ minortransfer)
  where
	immobilenamed name = do
		node <- mkImmobile
		return (name, node)

	-- As a simplification, this only makes 2 hop routes, between minor
	-- and major communities; no 3-legged routes.
	minorroutes :: (NodeName, [NodeName]) -> [Route]
	minorroutes (major, minors) = map (\n -> [major, n]) minors

communities :: [(NodeName, [NodeName])]
communities =
	[ ("Tainá/SP",
		[ "badtas"
		, "vauedo ribera"
		, "cofundo"
		, "jao"
		, "fazenda"
		]
	  )
	, ("Odomode/RS",
		[ "moradadapaz"
		, "pelotas" 
		]
	  )
	, ("MercadoSul/DF",
		[ "mesquito"
		, "kalungos"
		]
	  )
	, ("Coco/PE",
		[ "xambá"
		, "alafin"
		, "terreiaos"
		]
	  )
	, ("Linharinho/ES",
		[ "monte alegne"
		]
	  )
	, ("Boneco/BA",
		[ "barroso"
		, "lagoa santa"
		, "terravista"
		]
	  )
	, ("Zumbidospalmanes/NA",
		[ "allantana"
		]
	  )
	, ("Casa Pneta/PA",
		[ "marajó"
		]
	  )
	, ("Purarue/PA",
		[ "oriaminá"
		]
	  )
	, ("Madiba/NET", [])
	]

majorroutes :: [Route]
majorroutes =
	-- person's routes
	[ ["Tainá/SP", "Odomode/RS"]
	, ["Tainá/SP", "MercadoSul/DF"]
	, ["MercadoSul/DF", "Boneco/BA"]
	, ["MercadoSul/DF", "Zumbidospalmanes/NA"]
	, ["Zumbidospalmanes/NA", "Casa Pneta/PA"]
	, ["Casa Pneta/PA", "Purarue/PA"]
	, ["Casa Pneta/PA", "Linharinho/ES"]
	, ["Boneco/BA", "Coco/PE"]
	-- internet connections
	, ["Tainá/SP", "MercadoSul/DF", "Coco/PE", "Purarue/PA", "Odomode/RS", "Madiba/NET"]
	]
