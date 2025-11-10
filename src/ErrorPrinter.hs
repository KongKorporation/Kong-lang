module ErrorPrinter
    ( printIncludeError
    , printCompileError
    ) where

import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import Compiler.Include (IncludeError(..))
import Compiler.Type.Error (ProgramError(..), prettyError)

printInColor :: String -> String
printInColor str = "\ESC[91m" ++ str ++ "\ESC[0m"

-- Print an include error to stderr and exit
printIncludeError :: IncludeError -> IO ()
printIncludeError (FileNotFound path) =
    hPutStrLn stderr (printInColor("[Include error]") ++ " File not found: " ++ path) >> exitFailure
printIncludeError (CircularDependency file chain) =
    hPutStrLn stderr (printInColor("[Include error]") ++ " Circular dependency detected in " ++ file ++
                     ": " ++ show chain) >> exitFailure
printIncludeError (ParseError _ msg) =
    hPutStrLn stderr (printInColor("[Parsing error] ") ++ msg) >> exitFailure
printIncludeError (MissingInclude file missing) =
    hPutStrLn stderr (printInColor("[Include error]") ++ " File '" ++ file ++ "' includes '" ++ missing ++
                     "' but it was not provided in compilation arguments") >> exitFailure
printIncludeError (MissingSymbol requester included symbol) =
    hPutStrLn stderr (printInColor("[Include error]") ++ " File '" ++ requester ++ "' requests symbol '" ++ symbol ++
                     "' from '" ++ included ++ "' but it does not exist") >> exitFailure
printIncludeError (DuplicateSymbol symbol files) =
    hPutStrLn stderr (printInColor("[Include error]") ++ " Symbol '" ++ symbol ++ "' is defined in multiple files: " ++
                     show files) >> exitFailure

-- Print compilation errors to stderr and exit
printCompileError :: [ProgramError] -> IO ()
printCompileError errs =
    mapM_ (\(ProgramError file _ err) -> 
        hPutStrLn stderr $ printInColor("[Compilation error]") ++ " In file '" ++ file ++ "': " ++ prettyError err) errs
    >> exitFailure
