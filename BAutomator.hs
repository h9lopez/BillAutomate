import Data.Maybe

data Owner =
      Person        String
    | Account
    deriving (Show)

data Charge =
      Plan          Float
    | Equipment     Float
    | Misc          Float
    | TaxesFees     Float
    | Shared        Float
    deriving (Show)

data ItemizedCharges = ItemizedCharges Owner [Charge]
data TotaledCharges = TotaledCharges Owner Float deriving (Show)
data Bill = Bill [ItemizedCharges]


extractCharge :: Charge -> Float
extractCharge c = case c of
      Plan f        -> f
      Equipment f   -> f
      Misc f        -> f
      TaxesFees f   -> f
      Shared f      -> f
      otherwise     -> 0.0

calcTotals :: [Charge] -> Float
calcTotals cs =
    foldr ( \v a -> a + (extractCharge v) ) 0.0 cs

calculateSharedCharges :: Bill -> Float
calculateSharedCharges (Bill cs) =
        s / os
    where
        (s, os) =
                foldr (\(ItemizedCharges o cs) (shared, owners) -> case o of
                                                                    Account  -> (shared + calcTotals cs, owners)
                                                                    Person _ -> (shared, owners+1))
                        (0.0, 0) cs

calcShares :: Bill -> [TotaledCharges]
calcShares b@(Bill cs) =
    mapMaybe (\(ItemizedCharges o cs) -> case o of
                                            Person _    -> (Just (TotaledCharges o (sharedCharges + (calcTotals cs))))
                                            otherwise   -> Nothing ) cs
    where
        sharedCharges = calculateSharedCharges b


testBill = Bill [   (ItemizedCharges (Person "Abe") [Plan 45.0, Misc 13.0]),
                    (ItemizedCharges (Person "Don") [Plan 35.0, Misc 10.0]),
                    (ItemizedCharges (Account) [Misc 25.0])  ]

realBill = Bill [
                    (ItemizedCharges (Person "George") [Plan 10.0, Equipment 41.67, Misc 0.00, TaxesFees 3.01]),
                    (ItemizedCharges (Person "Obama") [Plan 25.0, Equipment 34.41, Misc 0.0, TaxesFees 3.72]),
                    (ItemizedCharges (Person "Al") [Plan 10.0, Equipment 40.74, Misc 0.0, TaxesFees 3.01]),
                    (ItemizedCharges (Person "Ronald") [Plan 10.0, Equipment 20.08, Misc 0.0, TaxesFees 3.01]),
                    (ItemizedCharges (Account) [Plan 100.0, Equipment 0.0, Misc 0.0, TaxesFees 4.62])
                ]

main = putStrLn (show (calcShares realBill))
