module Argsect.Pretty
(
    prettySwitches,
    prettyDataSwitches,
    prettyUndefText,
    prettyHelpText,
    prettyInvalidData,
    prettyInvalidText,
    prettyMissingRequiredDataSwitches
) where

import Argsect.Types

-- Gives a string showing switches in pretty form
prettySwitches :: [Switch] -> String
prettySwitches sws = foldl acc "" sws
    where
        acc :: Show a => String -> a -> String
        acc a b = a ++ "\n" ++ (show b)

-- Gives a string showing data switches in pretty form
prettyDataSwitches :: [DataSwitch] -> String
prettyDataSwitches dSws = foldl acc "" dSws
    where
        acc :: Show a => String -> a -> String
        acc a b = a ++ "\n" ++ (show b)

{-
(Undefined switches) (Defined switches)
Gives error text for having undefined switches / data switches
-}
prettyUndefText :: ([Switch], [DataSwitch])
                    -> ([Switch], [DataSwitch])
                    -> String
                    -> String
                    -> String
prettyUndefText undef defined progName usageDescription =
    "Error, used undefined switches:\n" ++ (prettySwitches $ fst undef) ++ "\n"
        ++ (prettyDataSwitches $ snd undef) ++ "\n" ++
        prettyHelpText (fst defined) (snd defined) progName usageDescription

-- Gives default help text based on arguments
prettyHelpText :: [Switch] -> [DataSwitch] -> String -> String -> String
prettyHelpText switches dSwitches progName usageDescription =
    "Usage: " ++ progName ++ " " ++ usageDescription ++ "\n\nSwitches:"
        ++ prettySwitches switches ++
        "\n\nData switches:" ++ prettyDataSwitches dSwitches

prettyInvalidText :: [DataSwitch] -> String -> String -> String
prettyInvalidText dsws progName usageDescription =
    "Usage: " ++ progName ++ " " ++ usageDescription ++ "\n\n" ++
    prettyInvalidData dsws

prettyInvalidData :: [DataSwitch] -> String
prettyInvalidData dsws =
    "Invalid data provided for the following data switches:\n\n" ++
        (foldl (\ acc dsw -> acc ++ "\n" ++ (go dsw)) "" dsws) ++
        "\nSee help text for usage."
    where
        go :: DataSwitch -> String
        go dsw =
            (dswIdShort dsw) ++ " " ++ (dswIdLong dsw) ++ " " ++ (dswInfo dsw)

prettyMissingRequiredDataSwitches :: [DataSwitch]
                                    -> [Switch]
                                    -> [DataSwitch]
                                    -> String
                                    -> String
                                    -> String
prettyMissingRequiredDataSwitches missing switches dataSwitches progName usage =
    "Missing on one or more required data switches:\n" ++
    (prettyDataSwitches missing) ++
    prettyHelpText switches dataSwitches progName usage
