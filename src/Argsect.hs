module Argsect
(
    module Argsect.Types,
    argsect,
    prettyHelpText,
    getDataFromDswList
) where

import Data.Maybe (fromMaybe)

import Argsect.Types
import Argsect.Pretty

{-
Either Right arguments or Left function that returns a function
taking the program name and usage description to return an error message to
show.
End the program after showing this message.
Takes the command line arguments and turns them into either Args or a function
taking the program name and usage information and returning an error string.
Parsing fails on an unrecognised switch being used, or data not validating.
-}

argsect :: [Switch]
           -> [DataSwitch]
           -> [String]
           -> Either (String -> String -> String) Args
argsect switches dataSwitches args =
    let
        unchecked = parseToUnchecked switches dataSwitches args
    in
        checkArgs switches dataSwitches unchecked

{-
Takes a list of strings, classifies them into positional arguments,
switches, and data switches. Returns them grouped into Args
-}
parseToUnchecked :: [Switch] -> [DataSwitch] -> [String] -> Args
parseToUnchecked switches dataSwitches args =
    Args posArgs
        (map (stringToSwitch switches) potSwitches)
        (map (stringToDataSwitch dataSwitches) potDataSwitches)
    where
            posArgs = [x | x <- args, ClPosArg == (classify x)]
            potSwitches = [x | x <- args, ClSwitch == (classify x)]
            potDataSwitches = [x | x <- args, ClDataSwitch == (classify x)]

{-
Checks if the arguments are valid where valid means:
    -- Add data switches pass the validation function
    -- All switches and data switches are defined
    -- All required data switches are present
If any arguments are not valid, a function taking the program name and the
usage description is returned, which will describe the issue to the user.
-}
checkArgs :: [Switch]
            -> [DataSwitch]
            -> Args
            -> Either (String -> String -> String) Args
checkArgs cSwitches cDataSwitches cArgs
    | not (null invalidDataSwitches) = Left invalidTextFun
    | not (null undefSwitches && null undefDataSwitches) =
        Left undefinedTextFun
    | not (null missing) = Left missingRequiredTextFun
    | otherwise = Right cArgs

    where
        -- Help texts
        undefinedTextFun =
            prettyUndefText
                (undefSwitches, undefDataSwitches) (cSwitches, cDataSwitches)
        invalidTextFun = prettyInvalidText cDataSwitches
        missingRequiredTextFun =
            prettyMissingRequiredDataSwitches missing cSwitches cDataSwitches

        -- Data that needs checked
        invalidDataSwitches = getInvalidDataSwitches (aDataSwitches cArgs)
        undefSwitches = getUndefSwitches (aSwitches cArgs)
        undefDataSwitches = getUndefDataSwitches (aDataSwitches cArgs)

        -- Find if there are any missing required data switches.
        requiredDataSwitches = filter (\ x -> dswRequired x) cDataSwitches
        missing =
            filter
                (\ x -> not (x `elem` (aDataSwitches cArgs)))
                requiredDataSwitches

-- Get all data switches where the data does not validate
getInvalidDataSwitches :: [DataSwitch] -> [DataSwitch]
getInvalidDataSwitches =
    filter (\ dsw -> not ((dswValidate dsw) (dswData dsw)))

-- Compares a string to the IDs of a switch
strCmpSwitch :: String -> Switch -> Bool
strCmpSwitch str sw = (str == (swIdShort sw)) || (str == (swIdLong sw))

-- Gets all undefined switches from a list
getUndefSwitches :: [Switch] -> [Switch]
getUndefSwitches = filter (\ x -> x == UndefinedSwitch "")

getUndefDataSwitches :: [DataSwitch] -> [DataSwitch]
getUndefDataSwitches = filter (\ x -> x == UndefinedDataSwitch "")

-- Splits a string on the first occurance of a Char
splitAtFirst :: Char -> String -> (String, String)
splitAtFirst x = fmap (drop 1) . break (x ==)

-- Head that doesn't throw exceptions, but returns a Maybe
head' :: [a] -> Maybe a
head' (x : _) = Just x
head' [] = Nothing

-- Classifies string into what type of argument it is
classify :: String -> ArgClassification
classify arg
    | '-' /= (head arg) = ClPosArg
    | '=' `elem` arg = ClDataSwitch
    | otherwise = ClSwitch

{-
If the specified ID is one of a data switch in the list, get the data from it,
else Nothing
-}
getDataFromDswList :: String -> [DataSwitch] -> Maybe String
getDataFromDswList dswid lst =
    maybe Nothing (\ x -> Just (dswData x))
        (head' matches)
    where
        matches =
            filter (\ x -> (dswIdShort x == dswid) || (dswIdLong x == dswid))
                lst

{-
Takes a list of defined switches and a potential switch.
Matches the potential switch to an element of the defined switches.
If its not found, then an UndefinedSwitch is returned.
-}
stringToSwitch :: [Switch] -> String -> Switch
stringToSwitch switches string =
    fromMaybe (UndefinedSwitch string) $ strTomSwitch switches string
    where
        strTomSwitch :: [Switch] -> String -> Maybe Switch
        strTomSwitch sSwitches sString =
            head' $ filter (strCmpSwitch sString) sSwitches

{-
Converts a string to a data switch, if its undefined UndefinedDataSwitch
is returned. Otherwise the match, with the data from the argument is returned.
-}
stringToDataSwitch :: [DataSwitch] -> String -> DataSwitch
stringToDataSwitch dSwitches string =
    let
        split = splitAtFirst '=' string
        match = getDataSwitchMatch dSwitches string
    in
        if isDataSwitchDefined match then
            DataSwitch
                (dswIdShort match)
                (dswIdLong match)
                (snd split)
                (dswValidate match)
                (dswRequired match)
                (dswInfo match)
        else
            match

-- Finds match in provided data switches or returns UndefinedDataSwitch
getDataSwitchMatch :: [DataSwitch] -> String -> DataSwitch
getDataSwitchMatch dSwitches str =
    fromMaybe (UndefinedDataSwitch "Undefined") (head' mMatch)
    where
        mMatch =
            filter
            (\ x -> (dswIdShort x == str) || (dswIdLong x == str))
            dSwitches
