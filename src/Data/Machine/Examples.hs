{-# LANGUAGE RankNTypes #-}

module Data.Machine.Examples where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.Machine
import Data.Maybe
import System.IO

-- this slurp slurps until an eof exception is raised.
slurpHandleBad :: Handle -> IO [String]
slurpHandleBad h = do
  s <- hGetLine h
  (s:) <$> slurpHandleBad h

-- this is the good slurp
-- it catches the exception, and cleans up.
slurpHandle :: Handle -> IO [String]
slurpHandle h = clean <$> slurp where
  clean = either (\(SomeException e) -> []) id
  slurp = try $ do { s <- hGetLine h; (s:) <$> slurpHandle h }

-- read a file, returning each line in a list 
readLines :: FilePath -> IO [String]
readLines f = withFile f ReadMode slurpHandle

-- | bad slurping machine
crashes :: Handle -> MachineT IO k String
crashes h = repeatedly $ lift (hGetLine h) >>= yield

-- | here is a plan that yields all the lines at once.
slurpHandlePlan :: Handle -> PlanT k [String] IO ()
slurpHandlePlan h = lift (slurpHandle h) >>= yield

{-
 - but we want a plan that will yield one line at a time
 - until we are done reading the file
 - but before we can do that, we need a few helper combinators.
 -}

-- | maybeYield is much like yield
-- | except that if there is no value to yield, it stops 
maybeYield :: Maybe o -> Plan k o ()
maybeYield = maybe stop yield

-- | exhaust runs a monadic action and continues to 
-- | yield its results, until it returns Nothing
exhaust :: Monad m => m (Maybe a) -> PlanT k a m ()
exhaust f = do (lift f >>= maybeYield); exhaust f

-- | getFileLines reads each line out of the given file
-- | and pumps them into the given process.
getFileLines :: FilePath -> ProcessT IO String a -> SourceT IO a 
getFileLines path proc = src ~> proc where 
  src :: SourceT IO String
  src = construct $ lift (openFile path ReadMode) >>= slurpLinesPlan
  slurpLinesPlan :: Handle -> PlanT k String IO ()
  slurpLinesPlan h = exhaust (clean <$> try (hGetLine h)) where
  clean = either (\(SomeException e) -> Nothing) Just

-- | lineCount counts the number of lines in a file
lineCount :: FilePath -> IO Int
lineCount path = head <$> (runT src) where
  src = getFileLines path (fold (\a b -> a + 1) 0)

-- | run a machine and just take the first value out of it.
runHead :: (Functor f, Monad f) => MachineT f k b -> f b
runHead src = head <$> runT src

-- | lineCharCount counts the number of lines, and characters in a file
lineCharCount :: FilePath -> IO (Int, Int)
lineCharCount path = runHead src where
  src = getFileLines path (fold (\(l,c) s -> (l+1, c + length s)) (0,0))

wordsProc :: Process String String
wordsProc = repeatedly $ do { s <- await; mapM_ yield (words s) }

printPlan :: Show a => PlanT (Is a) () IO ()
printPlan = await >>= lift . putStrLn . show >> yield ()

printProcess :: ProcessT IO String ()
printProcess = repeatedly printPlan

-- | a machine that prints all the lines in a file
printLines :: FilePath -> IO ()
printLines path = runT_ $ getFileLines path printProcess

-- | a machine that prints all the words in a file
printWords :: FilePath -> IO ()
printWords path = runT_ $ getFileLines path (wordsProc ~> printProcess)

-- | a machine that prints all the lines in a file,
-- | with the line numbers.
printLinesWithLineNumbers :: FilePath -> IO ()
printLinesWithLineNumbers path = runT_ (t ~> printProcess) where
  t :: TeeT IO Int String String
  t = tee (source [1..]) (getFileLines path echo) lineNumsT
  lineNumsT = repeatedly $ do
    i <- awaits L
    s <- awaits R
    yield $ show i ++ ": " ++ s

{-
def lineWordCount(fileName: String) =
  getFileLines(new File(fileName),
    (id split words) outmap (_.fold(_ => (1, 0), _ => (0, 1)))) execute

lineWordCount FilePath -> IO (Int, Int)
lineWordCount path = runHead lineWordCountSrc where
  lineWordCountSrc = echo 
-}

