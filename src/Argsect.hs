module Argsect 
(
    module Argsect.Types,
    argsect,
    defaultHelpText,
    defaultUndefText,
    getDataSwitchFromList
) where

import Data.Maybe

import Argsect.Types 
import Argsect.Pretty

-- First value is Just if all parsing was successful, the second value is Nothing
-- First value is Nothing if parsing failed, the second value is a Just function taking the
-- program name and usage description to return an error message to show. End the program after
-- showing this message.
-- Takes the command line arguments and turns them into either Args or a function taking the 
-- program name and usage information and returning an error string.
-- Parsing fails on an unrecognised switch being used, or data not validating.

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
    -- If any data does not validate, return a function taking progName 
    -- and usage that returns an error string
    | not (null invalidDataSwitches) = (Nothing, Just (defaultInvalidText cDataSwitches))
    -- If there are any undefined switches used, return a function taking progName 
    -- and usage that returns an error string
    | not (null undefSwitches && null undefDataSwitches) = (Nothing,
        Just (defaultUndefText (undefSwitches, undefDataSwitches) (cSwitches, cDataSwitches)))
    -- Otherwise parsing was successful :)
    | otherwise = (Just cArgs, Nothing)
    where
        invalidDataSwitches = getInvalidDataSwitches (aDataSwitches cArgs) :: [DataSwitch]
        undefSwitches = getUndefSwitches (aSwitches cArgs) :: [Switch]
        undefDataSwitches = getUndefDataSwitches (aDataSwitches cArgs) :: [DataSwitch]

-- Get all data switches where the data does not validate
getInvalidDataSwitches :: [DataSwitch] -> [DataSwitch]
getInvalidDataSwitches dsws = 
    filter (\dsw -> not ((dswValidate dsw) (dswData dsw)))
        dsws

-- Compares a string to the IDs of a switch
strCmpSwitch :: String -> Switch -> Bool
strCmpSwitch str sw = (str == (swIdShort sw)) || (str == (swIdLong sw))

-- Gets all undefined switches from a list
getUndefSwitches :: [Switch] -> [Switch]
getUndefSwitches = filter (\x -> x == UndefinedSwitch "")

getUndefDataSwitches :: [DataSwitch] -> [DataSwitch]
getUndefDataSwitches = filter (\x -> x == UndefinedDataSwitch "")

-- Splits a string on the first occurance of a Char
splitAtFirst :: Char -> String -> (String, String)
splitAtFirst x = fmap (drop 1) . break (x ==)  

-- Head that doesn't throw exceptions, but returns a Maybe
head' :: [a] -> Maybe a
head' (x:_) = Just x
head' [] = Nothing

-- Classifies string into what type of argument it is
classify :: String -> ArgClassification
classify arg
    | '-' /= (head arg) = ClPosArg
    | '=' `elem` arg = ClDataSwitch
    | otherwise = ClSwitch

-- If the specified ID is one of a data switch in the list, get the data from it, else Nothing
getDataFromDswList :: String -> [DataSwitch] -> Maybe String
getDataFromDswList dswid lst = maybe Nothing (\x -> Just (dswData x)) 
    (head' (filter (\x -> (dswIdShort x == dswid) || (dswIdLong x == dswid)) lst))

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
        -- Finds match in provided data switches or nothing
        getMatch :: [DataSwitch] -> String -> Maybe DataSwitch
        getMatch gdSwitches str = head' (filter (\x -> (dswIdShort x == str) || (dswIdLong x == str)) gdSwitches)

        split = splitAtFirst '=' string
        -- Converts a
        strTomSwitch :: [DataSwitch] -> String -> String -> Maybe DataSwitch
        strTomSwitch sdSwitches sString dat =
            -- Match is DataSwitch from defined
            if isJust $ getMatch sdSwitches sString then do
                match <- (getMatch sdSwitches sString)
                -- Same as match, just with the data from the argument
                Just (DataSwitch (dswIdShort match) (dswIdLong match) (dat) (dswValidate match) (dswrequired match) (dswInfo match))
            else
                Nothing