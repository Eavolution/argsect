# argsect
Argsect is a library for parsing command line arguments in Haskell.
## Usage
Command line arguments are parsed by argsect by passing a list of Switches and a list of command line arguments to `argsect :: [Switch] -> [String] -> Args`. 

A switch is a datatype containing a short id, long id, and description of the switch. for example:

`swHelp = Switch "-h" "--help" "Displays the help menu" :: Switch`

The parseArgs function returns Args where `type Args = ([PosArg])