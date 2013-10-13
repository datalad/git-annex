{- Remote costs.
 -
 - Copyright 2011-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Config.Cost where

{- We use a float for a cost to ensure that there is a cost in
 - between any two other costs. -}
type Cost = Float

{- Some predefined default costs.
 - Users setting costs in config files can be aware of these,
 - and pick values relative to them. So don't change. -}
cheapRemoteCost :: Cost
cheapRemoteCost = 100
nearlyCheapRemoteCost :: Cost
nearlyCheapRemoteCost = 110
semiExpensiveRemoteCost :: Cost
semiExpensiveRemoteCost = 175
expensiveRemoteCost :: Cost
expensiveRemoteCost = 200
veryExpensiveRemoteCost :: Cost
veryExpensiveRemoteCost = 1000

{- Adjusts a remote's cost to reflect it being encrypted. -}
encryptedRemoteCostAdj :: Cost
encryptedRemoteCostAdj = 50

{- Given an ordered list of costs, and the position of one of the items
 - the list, inserts a new cost into the list, in between the item
 - and the item after it.
 -
 - If two or move items have the same cost, their costs are adjusted
 - to make room. The costs of other items in the list are left
 - unchanged.
 -
 - To insert the new cost before any other in the list, specify a negative
 - position. To insert the new cost at the end of the list, specify a
 - position longer than the list.
 -}
insertCostAfter :: [Cost] -> Int -> [Cost]
insertCostAfter [] _ = []
insertCostAfter l pos
	| pos < 0 = costBetween 0 (l !! 0) : l
	| nextpos > maxpos = l ++ [1 + l !! maxpos]
	| item == nextitem =
		let (_dup:new:l') = insertCostAfter lastsegment 0
		in firstsegment ++ [costBetween item new, new] ++ l'
	| otherwise =
		firstsegment ++ [costBetween item nextitem ] ++ lastsegment
  where
  	nextpos = pos + 1
	maxpos = length l - 1
	
	item = l !! pos
	nextitem = l !! nextpos
		
	(firstsegment, lastsegment) = splitAt (pos + 1) l

costBetween :: Cost -> Cost -> Cost
costBetween x y
	| x == y = x
	| x > y = -- avoid fractions unless needed
		let mid = y + (x - y) / 2
		    mid' = fromIntegral (floor mid :: Int)
		in if mid' > y then mid' else mid
	| otherwise = costBetween y x

{- Make sure the remote cost numbers work out. -}
prop_cost_sane :: Bool
prop_cost_sane = False `notElem`
	[ expensiveRemoteCost > 0
	, cheapRemoteCost < nearlyCheapRemoteCost
	, nearlyCheapRemoteCost < semiExpensiveRemoteCost
	, semiExpensiveRemoteCost < expensiveRemoteCost
	, cheapRemoteCost + encryptedRemoteCostAdj > nearlyCheapRemoteCost
	, nearlyCheapRemoteCost + encryptedRemoteCostAdj < semiExpensiveRemoteCost
	, nearlyCheapRemoteCost + encryptedRemoteCostAdj < expensiveRemoteCost
	]
