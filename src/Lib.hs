module Lib
  ( report
  ) where

import           Data.List
import           Numeric.Natural

report = intercalate "\n" [(show turns), (show $ game_state turns)]

turns =
  [ AddCashflowShape (PeriodicCashflow days_in_a_month 15 3)
  , AdvanceDays 1
  , AddCashflowShape (PeriodicCashflow days_in_a_month 15 3)
  ]

type HowManyTimes = Natural

type HowManyDays = Natural

type Cash = Integer

days_in_a_month = 20 :: HowManyDays

peridoc_cashflow :: HowManyDays -> Cash -> HowManyDays -> Cashflow
peridoc_cashflow how_often how_much how_many_months = monthly_cash_series
  where
    monthly_cash_series :: Cashflow
    monthly_cash_series =
      concat $ replicate (fromEnum how_many_months) monthly_cash
    monthly_cash :: Cashflow
    monthly_cash =
      map
        (\(_, cash) -> cash)
        (zip [1 .. days_in_a_month] $ [how_much] ++ (repeat 0))

new_gamestate :: GameState -> Turn -> GameState
new_gamestate gs turn =
  case turn of
    (AdvanceDays how_many_days) ->
      gs {days_ellapsed = how_many_days + days_ellapsed gs}
    (AddCashflow additional_cashflow) ->
      gs
        { cumulative_cashflow =
            (superimpose_cashflow
               (days_ellapsed gs)
               additional_cashflow
               (cumulative_cashflow gs))
        }
    (AddCashflowShape additional_cashflowshape) ->
      gs
        { cumulative_cashflow =
            (superimpose_cashflow
               (days_ellapsed gs)
               (cashflowshape2cashflow additional_cashflowshape)
               (cumulative_cashflow gs))
        }

cashflowshape2cashflow (PeriodicCashflow how_often how_much how_many_times) =
  peridoc_cashflow how_often how_much how_many_times

superimpose_cashflow initial_dead_days additional_cashflow orig_cashflow =
  cashflow
  where
    initial_dead_period = take (fromEnum initial_dead_days) $ repeat 0
    cash_pairs :: [(Cash, Cash)]
    cash_pairs =
      zip
        (initial_dead_period ++ additional_cashflow)
        (orig_cashflow ++ (repeat 0))
    cashflow :: Cashflow
    cashflow = map (\(x, y) -> x + y) cash_pairs

game_state :: [Turn] -> GameState
game_state turns = foldl new_gamestate (GameState 0 []) turns

data CashflowShape =
  PeriodicCashflow HowManyDays
                   Cash
                   HowManyTimes
  deriving (Show, Eq)

data Turn
  = AddCash Cash
  | AddCashflow Cashflow
  | AddCashflowShape CashflowShape
  | AdvanceDays HowManyDays
  | Noop
  deriving (Show, Eq)

data GameState = GameState
  { days_ellapsed       :: HowManyDays
  , cumulative_cashflow :: Cashflow
  } deriving (Show)

type Cashflow = [Cash]
