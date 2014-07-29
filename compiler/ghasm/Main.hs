module Main (main) where

import System.Environment (getArgs)

import Parser
import Compile

-- Command line options

import System.Console.GetOpt

data Flag = Optimize
	  | Optimize2
	  | ParseOnly
	  | Usage
	  deriving (Eq, Show)

options  :: [OptDescr Flag]
options  =
   [ Option ['O'] ["optimize"] (NoArg Optimize) "make it run faster",
     Option ['X'] ["optimize2"] (NoArg Optimize2) "make it run much faster",
     Option ['p'] ["parse-only"] (NoArg ParseOnly) "don't write any output files",
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

--         putStr (unlines (map (flip displayDefn "\n") ds'))
--       print (lex text)


fileHasExtension	:: String -> Bool
fileHasExtension f	=  case fileParse f of
			      (_,_,"") -> False
			      _ -> True

-- Parse file into path, basename, and extension.
fileParse :: String -> (String, String, String)
fileParse f = ge "" "" (reverse f)
   where
      ge e  r ""	=  ("", r, e)
      ge "" r ('.':s)   =  ge r "" s
      ge e  r ('.':s)   =  ge e ('.':r) s
      ge e  r (c:s)	=  if isPathSep c then
			      (reverse (if retainPathSep c then c:s else s),
			       r, e)
			   else
			       ge e (c:r) s

isPathSep		:: Char -> Bool
isPathSep '/'		=  True
isPathSep '\\'		=  True
isPathSep ':'		=  True
isPathSep _		=  False

retainPathSep		:: Char -> Bool
retainPathSep ':'	=  True
retainPathSep _		=  False

