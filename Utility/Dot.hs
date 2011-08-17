{- a simple graphviz / dot(1) digraph description generator library
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Dot where -- import qualified

{- generates a graph description from a list of lines -}
graph :: [String] -> String
graph s = unlines $ [header] ++ map indent s ++ [footer]
	where
		header = "digraph map {"
		footer= "}"

{- a node in the graph -}
graphNode :: String -> String -> String
graphNode nodeid desc = label desc $ quote nodeid

{- an edge between two nodes -}
graphEdge :: String -> String -> Maybe String -> String
graphEdge fromid toid desc = indent $ maybe edge (`label` edge) desc
	where
		edge = quote fromid ++ " -> " ++ quote toid

{- adds a label to a node or edge -}
label :: String -> String -> String
label = attr "label"

{- adds an attribute to a node or edge
 - (can be called multiple times for multiple attributes) -}
attr :: String -> String -> String -> String
attr a v s = s ++ " [ " ++ a ++ "=" ++ quote v ++ " ]"

{- fills a node with a color -}
fillColor :: String -> String -> String
fillColor color s = attr "fillcolor" color $ attr "style" "filled" s

{- apply to graphNode to put the node in a labeled box -}
subGraph :: String -> String -> String -> String -> String
subGraph subid l color s =
	"subgraph " ++ name ++ " {\n" ++
		ii setlabel ++
		ii setfilled ++
		ii setcolor ++
		ii s ++
		indent "}"
	where
		-- the "cluster_" makes dot draw a box
		name = quote ("cluster_" ++ subid)
		setlabel = "label=" ++ quote l
		setfilled = "style=" ++ quote "filled"
		setcolor = "fillcolor=" ++ quote color
		ii x = indent (indent x) ++ "\n"

indent ::String -> String
indent s = '\t' : s

quote :: String -> String
quote s = "\"" ++ s' ++ "\""
	where
		s' = filter (/= '"') s
