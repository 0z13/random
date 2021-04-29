{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NoStarIsType #-}

import Data.Either

newtype Temp unit = Temp Double
  deriving (Num, Fractional)

data F
data C

paperBurning :: Temp F
paperBurning = 451

absoluteZero :: Temp C
absoluteZero = -273.15



