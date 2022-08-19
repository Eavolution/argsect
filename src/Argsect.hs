module Argsect  (
                module Types,
                argsect,
                defaultHelpText,
                defaultUndefText,
                getUndefDataSwitches,
                getUndefSwitches
                ) where

import Types 
import Data.Maybe

-- Takes the command line arguments and turns them into Maybe Args.
-- If unrecognised switch etc, Just (String -> String -> String) is returned.
-- This function expects the program name and usage description
-- and returns the formatted error text to be displayed
argsect :: [Switch] -> [DataSwitch] -> [String] -> (Maybe Args, Maybe (String -> String -> String))
argsect switches dataSwitches args = checkArgs switches dataSwitches uncheckedArgs
    
        where
            -- lstTuple is a tuple of list of positional arguments, potentially defined switches
            posArgs = [x | x <- args, ClPosArg == (classify x)] :: [String]
            potSwitches = [x | x <- args, ClSwitch == (classify x)] :: [String]
            potDataSwitches = [x | x <- args, ClDataSwitch == (classify x)] :: [String]

            uncheckedArgs :: Args
            uncheckedArgs = Args posArgs (map (stringToSwitch switches) potSwitches)
                (map (stringToDataSwitch dataSwitches) potDataSwitches)

            checkArgs :: [Switch] -> [DataSwitch] -> Args -> (Maybe Args, Maybe (String -> String -> String))
            checkArgs cSwitches cDataSwitches cArgs
                | null undefSwitches && null undefDataSwitches = (Just cArgs, Nothing)
                | otherwise = (Nothing,
                    Just (defaultUndefText (undefSwitches, undefDataSwitches) (cSwitches, cDataSwitches)))
                where
                    undefSwitches = getUndefSwitches cSwitches :: [Switch]
                    undefDataSwitches = getUndefDataSwitches cDataSwitches :: [DataSwitch]

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

-- Gets all undefined values from a list
getUndefSwitches :: [Switch] -> [Switch]
getUndefSwitches = filter (\x -> x == UndefinedSwitch "")

getUndefDataSwitches :: [DataSwitch] -> [DataSwitch]
getUndefDataSwitches = filter (\x -> x == UndefinedDataSwitch "")

-- Compares a string to the IDs of a switch
strCmpSwitch :: String -> Switch -> Bool
strCmpSwitch str sw = (str == (swIdShort sw)) || (str == (swIdLong sw))

-- Head that doesn't throw exceptions, but returns a Maybe
head' :: [a] -> Maybe a
head' (x:_) = Just x
head' [] = Nothing

-- Splits a string on the first occurance of a Char
splitAtFirst :: Char -> String -> (String, String)
splitAtFirst x = fmap (drop 1) . break (x ==)    

-- Takes a list of defined switches and a potential switch. Matches the potential switch to an element
-- of the defined switches. If its not found, then an UndefinedSwitch is returned.
stringToSwitch :: [Switch] -> String -> Switch
stringToSwitch switches string = fromMaybe (UndefinedSwitch string) $ strTomSwitch switches string
    where 
        -- Converts a string to a Maybe Switches. If the string is a defined switch, then the Just of
        -- that is returned. Otherwise Nothing is returned
        strTomSwitch :: [Switch] -> String -> Maybe Switch
        strTomSwitch sSwitches sString = head' $ filter (strCmpSwitch sString) sSwitches

stringToDataSwitch :: [DataSwitch] -> String -> DataSwitch
stringToDataSwitch dSwitches string = fromMaybe (UndefinedDataSwitch string) (strTomSwitch dSwitches (fst split) (snd split))
    where
        getMatch :: [DataSwitch] -> String -> Maybe DataSwitch
        getMatch gdSwitches str = head' (filter (\x -> (dswIdShort x == str) || (dswIdLong x == str)) gdSwitches)

        split = splitAtFirst '=' string
        strTomSwitch :: [DataSwitch] -> String -> String-> Maybe DataSwitch
        strTomSwitch sdSwitches sString dat =
            if isJust $ getMatch sdSwitches sString then do
                match <- (getMatch sdSwitches sString)
                Just (DataSwitch (dswIdShort match) (dswIdLong match) (Just dat) (dswrequired match) (dswInfo match))
            else
                Nothing

-- Classifies string into what type of argument it is
classify :: String -> ArgClassification
classify arg
    | '-' /= (head arg) = ClPosArg
    | '=' `elem` arg = ClDataSwitch
    | otherwise = ClSwitch