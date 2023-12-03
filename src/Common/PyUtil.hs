module Common.PyUtil where

import System.Process
import System.IO

python :: String -> IO Int
python s = do
    (_, Just hout, _, _) <- createProcess (shell $ "python " ++ s) { std_out = CreatePipe }
    output <- hGetContents hout
    return (read output :: Int)