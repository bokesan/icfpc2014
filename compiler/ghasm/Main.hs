module Main (main) where

import System.Environment (getArgs)

import Parser
import Compile

-- Command line options

import System.Console.GetOpt

data Flag = ParseOnly
	  | Usage
	  deriving (Eq, Show)

options  :: [OptDescr Flag]
options  =
   [ Option ['p'] ["parse-only"] (NoArg ParseOnly) "don't write any output files",
     Option ['h','?'] ["help"] (NoArg Usage) "this text" ]


main  :: IO ()
main  =  getArgs >>= \argv ->
	 case getOpt Permute options argv of
	    (o,n,[])   -> if Usage `elem` o then
			     usage
			  else
			     fcMain o n
	    (_,_,errs) -> putStr ("error: " ++ concat errs) >>
			  usage

usage :: IO ()
usage =  putStr (usageInfo "usage: fc [OPTION...] file.." options)


fcMain :: [Flag] -> [String] -> IO ()
fcMain _    []		=  return ()
fcMain opts (f:fs)	=  compile opts f >> fcMain opts fs


compile :: [Flag] -> String -> IO ()
compile opts f =
   do
      text <- readFile f
      let ds = parseProgram f text
          ds' | ParseOnly `elem` opts = []
              | otherwise = compileProgram ds
       in
         if ParseOnly `elem` opts then
           mapM_ (\d -> putStrLn (show d)) ds
         else
           mapM_ (\d -> putStrLn (show d)) ds'
