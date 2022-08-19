-- Holds all the types and instances needed to parse arguments

module Types    (
                PosArg,
                ArgClassification (..),
                Args (..),
                Switch (..),
                DataSwitch (..)
                ) where 

import Data.Maybe

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
    show (Args pos sw dsw) = "Positionals: " ++ (show pos) ++ "\nSwitches: " ++ (show sw) ++ "\nData switches: " ++ (show dsw)

-- Represents a switch containing no data ie. Switch "-h" "--help" "Displays the help menu."
data Switch =   UndefinedSwitch {swId :: String} |
                Switch  {
                        swIdShort :: String,
                        swIdLong :: String,
                        swInfo :: String
                        }

instance Show Switch where
    show (Switch idS idL inf) = 
        idS ++ " " ++ idL ++ " -> " ++ inf
    show (UndefinedSwitch switchId)=
        "Undefined switch: " ++ switchId

instance Eq Switch where
    -- If one of the id's are the same, the switch is the same
    (==) (Switch idS1 idL1 _) (Switch idS2 idL2 _) = (idS1 == idS2) || (idL1 == idL2)
    -- All undefineds are the same
    (==) (UndefinedSwitch _) (UndefinedSwitch _) = True
    -- If they're different, theyre not equivalent
    (==) _ _ = False

-- Represents a data switch (for example --wordlist=/usr/share/wordlists/rockyou.lst)
data DataSwitch = UndefinedDataSwitch {dswId :: String} |
                  DataSwitch {
                            dswIdShort :: String,
                            dswIdLong :: String,
                            dswData :: Maybe String, -- The data for the switch, after the =. Provide an example 
                            dswrequired :: Bool, -- If the switch is required
                            dswInfo :: String
                            }


instance Show DataSwitch where
    show (DataSwitch idS idL dat req inf) = 
        idS ++ " " ++ idL ++ (maybe "" (\x -> " = " ++ x) dat) ++ " -> " ++ inf ++ " Required = " ++ show req
    show (UndefinedDataSwitch switchId) = "Undefined data-switch: " ++ switchId

instance Eq DataSwitch where
    -- If one of the id's are the same, the switch is the same
    (==) (DataSwitch idS1 idL1 _ _ _) (DataSwitch idS2 idL2 _ _ _) = (idS1 == idS2) || (idL1 == idL2)
    -- All undefineds are the same
    (==) (UndefinedDataSwitch _) (UndefinedDataSwitch _) = True
    -- If they're different, theyre not equivalent
    (==) _ _ = False