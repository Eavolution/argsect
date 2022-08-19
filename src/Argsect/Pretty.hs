module Argsect.Pretty   
(
    prettySwitches,
    prettyDataSwitches,
    defaultUndefText,
    defaultHelpText,
    prettyInvalidData,
    defaultInvalidText
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

-- (Undefined switches) (Defined switches)
-- Gives error text for having undefined switches / data switches
defaultUndefText :: ([Switch], [DataSwitch]) -> ([Switch], [DataSwitch]) -> String -> String -> String
defaultUndefText undef defined progName usageDescription =
    "Error, used undefined switches:\n" ++ (prettySwitches $ fst undef) ++ "\n" ++
        (prettyDataSwitches $ snd undef) ++ "\n" ++
        defaultHelpText (fst defined) (snd defined) progName usageDescription

-- Gives default help text based on arguments
defaultHelpText :: [Switch] -> [DataSwitch] -> String -> String -> String
defaultHelpText switches dSwitches progName usageDescription = 
    "Usage: " ++ progName ++ " " ++ usageDescription ++ "\n\nSwitches:"
        ++ prettySwitches switches ++ "\n\nData switches:\n" ++ prettyDataSwitches dSwitches

defaultInvalidText :: [DataSwitch] -> String -> String -> String
defaultInvalidText dsws progName usageDescription =
    "Usage: " ++ progName ++ " " ++ usageDescription ++ "\n\n" ++ prettyInvalidData dsws

prettyInvalidData :: [DataSwitch] -> String
prettyInvalidData dsws =
    "Invalid data provided for the following data switches:\n\n" ++
        (foldl (\acc dsw -> acc ++ "\n" ++ (go dsw)) "" dsws) ++
        "\nSee help text for usage."
    where 
        go :: DataSwitch -> String
        go dsw =
            (dswIdShort dsw) ++ " " ++ (dswIdLong dsw) ++ " " ++ (dswInfo dsw)