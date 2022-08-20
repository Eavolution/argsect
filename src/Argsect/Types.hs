-- Holds all the types and instances needed to parse arguments

module Argsect.Types
(
    PosArg,
    ArgClassification (..),
    Args (..),
    Switch (..),
    DataSwitch (..),
    isSwitchDefined,
    isDataSwitchDefined
) where

type PosArg = String

-- Describes what a string is
data ArgClassification = ClSwitch | ClDataSwitch | ClPosArg deriving Eq

-- Represents the command line arguments
data Args = Args {
            aPositionals :: [PosArg],
            aSwitches :: [Switch],
            aDataSwitches :: [DataSwitch]
            }

instance Show Args where
    show (Args pos sw dsw) =
        "Positionals: " ++ (show pos) ++ "\nSwitches: " ++ (show sw) ++
        "\nData switches: " ++ (show dsw)

{-
Represents a switch containing no data
ie. Switch "-h" "--help" "Displays the help menu."
-}
data Switch = UndefinedSwitch {swId :: String} |
                Switch {
                       -- Short id ie. -h
                       swIdShort :: String,
                       -- Long id ie. --help
                       swIdLong :: String,
                       -- Information ie. Displays the help menu
                       swInfo :: String
                       }

isSwitchDefined :: Switch -> Bool
isSwitchDefined (UndefinedSwitch "") = False
isSwitchDefined _ = True

instance Show Switch where
    show (Switch idS idL inf) =
        idS ++ " " ++ idL ++ " -> " ++ inf
    show (UndefinedSwitch switchId) =
        "Undefined switch: " ++ switchId

instance Eq Switch where
    -- Two switches are the same if either ID matches
    (==) (Switch idS1 idL1 _) (Switch idS2 idL2 _) =
        (idS1 == idS2) || (idL1 == idL2)
    -- Undefined switches are always the same
    (==) (UndefinedSwitch _) (UndefinedSwitch _) = True
    -- Different types are different
    (==) _ _ = False

{-
Represents a data switch
(for example --wordlist=/usr/share/wordlists/rockyou.lst)
-}
data DataSwitch = UndefinedDataSwitch {dswId :: String} |
                  DataSwitch {
                            -- Short id ie. -h
                            dswIdShort :: String,
                            -- Long id ie. --help
                            dswIdLong :: String,
                            {-
                            The data for the switch, after the =.
                            Provide an example ie. /usr/wordlist/rockyou.lst
                            -}
                            dswData :: String,
                            -- Data validation ie. (\ x -> length x == 5)
                            dswValidate :: (String -> Bool),
                            -- If the switch is required
                            dswRequired :: Bool,
                            -- Information ie. Displays the help menu
                            dswInfo :: String
                            }

isDataSwitchDefined :: DataSwitch -> Bool
isDataSwitchDefined (UndefinedDataSwitch "") = False
isDataSwitchDefined _ = True

instance Show DataSwitch where
    show (DataSwitch idS idL dat _ req inf) =
        idS ++ " " ++ idL ++ " = " ++ dat ++ " -> " ++ inf ++
        " Required = " ++ show req
    show (UndefinedDataSwitch switchId) =
        "Undefined data-switch: " ++ switchId

instance Eq DataSwitch where
    -- If one of the id's are the same, the switch is the same
    (==) (DataSwitch idS1 idL1 _ _ _ _) (DataSwitch idS2 idL2 _ _ _ _) =
        (idS1 == idS2) || (idL1 == idL2)
    -- All undefineds are the same
    (==) (UndefinedDataSwitch _) (UndefinedDataSwitch _) = True
    -- If they're different, theyre not equivalent
    (==) _ _ = False
