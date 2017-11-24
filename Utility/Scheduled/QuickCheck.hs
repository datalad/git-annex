{- quickcheck for scheduled activities
 - 
 - Copyright 2013-2014 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Utility.Scheduled.QuickCheck where

import Utility.Scheduled
import Utility.QuickCheck

import Control.Applicative
import Prelude

instance Arbitrary Schedule where
	arbitrary = Schedule <$> arbitrary <*> arbitrary

instance Arbitrary ScheduledTime where
	arbitrary = oneof
		[ pure AnyTime
		, SpecificTime 
			<$> choose (0, 23)
			<*> choose (1, 59)
		]

instance Arbitrary Recurrance where
	arbitrary = oneof
		[ pure Daily
		, Weekly <$> arbday
		, Monthly <$> arbday
		, Yearly <$> arbday
		, Divisible
			<$> positive arbitrary
			<*> oneof -- no nested Divisibles
				[ pure Daily
				, Weekly <$> arbday
				, Monthly <$> arbday
				, Yearly <$> arbday
				]
		]
	  where
		arbday = oneof
			[ Just <$> nonNegative arbitrary
			, pure Nothing
			]

prop_schedule_roundtrips :: Schedule -> Bool
prop_schedule_roundtrips s = toSchedule (fromSchedule s) == Just s
