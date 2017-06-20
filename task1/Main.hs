module Main where
import MLCommon
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.List as List
import System.IO
import System.Environment
    
axs = classicAxiomList

annotateC = annotate axs
    
main = do
    args <- getArgs
    let al = length args in
        (if (al == 0) then ((return stdin) >>=) else (withFile (args !! 0) ReadMode))
            (\hIn -> (if (al <= 1) then ((return stdout) >>=) else (withFile (args !! 1) WriteMode))
                (\hOut -> do
                    head <- hGetLine hIn
                    hPutStrLn hOut head
                    inputIter hIn hOut (Map.empty) 1 [] (fst $ parseHeader head)
                )
            )
    return ()
    
    

inputIter hIn hOut sts it listcache ass = do
    end <- hIsEOF hIn
    if (end)
        then return ()
        else do
            line <- hGetLine hIn
            hPutStrLn hOut $ annotateC ass sts listcache it line
            let parsed = parseStatement line in
                inputIter hIn hOut (Map.insert parsed it sts) (it + 1) ((parsed, it):listcache) ass
            return ()
