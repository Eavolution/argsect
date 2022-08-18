module Argsect where
import Types

import Data.Maybe

-- Testing
swHelp = Switch "-h" "--help" "Displays the help menu."
swEu = Switch "-e" "--europe" "Formats EU style."

dswWords = DataSwitch "-w" "--wordlist" Nothing True "Sets wordlist."
parse = argsect [swHelp, swEu] [dswWords]
-- /Testing

getUndefined :: [Switch] -> [Switch]
getUndefined = filter (\x -> x == UndefinedSwitch "")

strCmpSwitch :: String -> Switch -> Bool
strCmpSwitch str sw = (str == (swIdShort sw)) || (str == (swIdLong sw))

-- Head that doesn't throw exceptions, but returns a Maybe
head' :: [a] -> Maybe a
head' (x:_) = Just x
head' [] = Nothing

-- Strip stripVal from start of list
lstrip :: (Eq a) => a -> [a] -> [a]
lstrip stripVal lst = 
    case lst of
        [] -> []
        (x:xs) -> if x == stripVal
        then lstrip stripVal xs else lst

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
        strTomSwitch switches string = head' $ filter (strCmpSwitch string) switches

stringToDataSwitch :: [DataSwitch] -> String -> DataSwitch
stringToDataSwitch dSwitches string = fromMaybe (UndefinedDataSwitch string) (strTomSwitch dSwitches (fst split) (snd split))
    where
        getMatch :: String -> Maybe DataSwitch
        getMatch str = head' (filter (\x -> (dswIdShort x == str) || (dswIdLong x == str)) dSwitches)

        split = splitAtFirst '=' string
        strTomSwitch :: [DataSwitch] -> String -> String-> Maybe DataSwitch
        strTomSwitch switches string dat =
            if isJust $ getMatch string then do
                match <- (getMatch string)
                Just (DataSwitch (dswIdShort match) (dswIdLong match) (Just     dat) (dswrequired match) (dswInfo match))
            else
                Nothing

-- Classifies string into what type of argument it is
classify :: String -> ArgClassification
classify arg
    | '-' /= (head arg) = ClPosArg
    | '=' `elem` arg = ClDataSwitch
    | otherwise = ClSwitch

-- Takes the command line arguments and turns them into Args
argsect :: [Switch] -> [DataSwitch] -> [String] -> Args
argsect switches dataSwitches args =
    Args posArgs (map (stringToSwitch switches) potSwitches) (map (stringToDataSwitch dataSwitches) potDataSwitches)
        where
            -- lstTuple is a tuple of list of positional arguments, potentially defined switches
            posArgs = [x | x <- args, ClPosArg == (classify x)]
            potSwitches = [x | x <- args, ClSwitch == (classify x)]
            potDataSwitches = [x | x <- args, ClDataSwitch == (classify x)]
        