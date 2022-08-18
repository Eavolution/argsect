module Argsect where

data Switch = 
    UndefinedSwitch {id :: String} |
    Switch { 
            idShort :: String,
            idLong :: String,
            info :: String
            }

instance Show Switch where
    show (Switch idS idL inf) = 
        idS ++ " = " ++ idL ++ " -> " ++ inf
    show (UndefinedSwitch switchId) = "Undefined switch = " ++ switchId

instance Eq Switch where
    -- If one of the id's are the same, the switch is the same
    (==) (Switch idS1 idL1 _) (Switch idS2 idL2 _) = (idS1 == idS2) || (idL1 == idL2)
    -- All undefineds are the same
    (==) (UndefinedSwitch _) (UndefinedSwitch _) = True
    -- If they're different, theyre not equivalent
    (==) _ _ = False

type PosArg = String

-- Arguments, fst positional in order, snd switches 
type Args = ([PosArg], [Switch])

getUndefined :: [Switch] -> [Switch]
getUndefined = filter (\x -> x == UndefinedSwitch "")

strCmpSwitch :: String -> Switch -> Bool
strCmpSwitch str sw = (str == (idShort sw)) || (str == (idLong sw))

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
        
isSwitch :: String -> Bool
isSwitch x
    | null x = False
    | head x == '-' = True
    | otherwise = False
    
-- Takes a list of defined switches and a potential switch. Matches the potential switch to an element
-- of the defined switches. If its not found, then an UndefinedSwitch is returned.
stringToSwitch :: [Switch] -> String -> Switch
stringToSwitch switches string = maybe (UndefinedSwitch string) (\x -> x) $ strTomSwitch switches string
    where 
        -- Converts a string to a Maybe Switches. If the string is a defined switch, then the Just of
        -- that is returned. Otherwise Nothing is returned
        strTomSwitch :: [Switch] -> String -> Maybe Switch
        strTomSwitch switches string = head' $ filter (strCmpSwitch string) switches


-- Takes the command line arguments and turns them into Args
argsect :: [Switch] -> [String] -> Args
argsect switches args = (fst lstTuple, stringsToSwitches switches potSwitches)
    where 
        stringsToSwitches :: [Switch] -> [String] -> [Switch]
        stringsToSwitches sws = map (stringToSwitch sws)

        -- lstTuple is a tuple of list of positional arguments and potentially defined switches
        posArgs = [x | x <- args, not $ isSwitch x]
        potSwitches = [y | y <- args, isSwitch y]
        lstTuple = (posArgs, potSwitches) :: ([String], [String])
        