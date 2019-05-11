module L1105
    where

import qualified Data.Map as Map

data LockerState = Taken | Free deriving(Show,Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

fetchCode :: Int -> LockerMap -> Either String Code
fetchCode lockerNo map =
    case Map.lookup lockerNo map of
        Nothing -> Left $ "Locker no" ++ show lockerNo ++ "doesnt ex"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "LLocker no" ++ show lockerNo ++  " taken "
