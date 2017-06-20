import System.Environment
import System.IO
import Control.Monad

import ParserCombinators
import MDParse
import HTMLGen

data OutputFormat = HTML

-- outputName :: OutputFormat -> String
-- outputName HTML = "output.html"

errorLogFile :: String
errorLogFile = "error.log"
-- outname = "ex.html"
main = do
  [fname, "-o", outname] <- getArgs
  raw <- readFile fname
  -- putStrLn $ "\nGenerated html, will be written to " ++ outname ++ ": "
  let tree = fst . head $ parse doc raw
  -- print raw
  -- print tree
  -- putStrLn $ "Parsed markdown: " ++ show(tree)
  let html = generateHTML outname tree
      --print $ html
  writeFile outname html
