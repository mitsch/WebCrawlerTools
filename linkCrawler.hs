module Main where
	import Data.Bool
	import Data.List
	import Data.String.Utils
	import Control.Concurrent( forkIO )
	import Control.Concurrent( threadDelay )
	import Control.Concurrent.Chan
	import Control.Monad
	import System( getArgs )
	import System.FilePath
	import System.IO
	import CommandLine
	import Crawler


	while :: IO Bool -> IO ()
	while action = do
		continue <- action
		case continue of {True -> while action; False -> return ()}

	newHandler :: (Chan b -> a -> IO ()) -> Maybe FilePath -> (b -> String) -> IO (Manager -> a -> IO ())
	newHandler _ Nothing _ = return $ (\m t -> return ())
	newHandler c (Just path) f = do
		channel <- newChan
		id <- forkIO $ openFile path AppendMode >>= (\handle -> forever $ readChan channel >>= (\t -> hPutStrLn handle (f t)))
		return $ (\m t -> c channel t)

	newSingleHandler = newHandler writeChan
	newListHandler = newHandler writeList2Chan

	foreverInDelay :: Int -> IO a -> IO ()
	foreverInDelay delay action = forever $ action >> threadDelay delay
	-- ----------------------------------------------------------------------------
	main = do
		arguments <- getArgs
		compilationResult <- compileConsoleArguments arguments
		case compilationResult of
			CompilationFailure message -> mapM_ putStrLn message
			CompilationSuccess seeds rejectedSuffixes workers externalOut invalidOut rejectOut visitedOut httpOutput internetOutputs -> do
				invalidHandler <- newSingleHandler invalidOut (\item -> let (url, contentTypes) = item in url ++ " " ++ contentTypes)
				visitedHandler <- newListHandler visitedOut show 
				externalHandler <- newListHandler externalOut show
				rejectedHandler <- newListHandler rejectOut show
				httpHandler <- newSingleHandler httpOutput show
				internetHandler <- newSingleHandler internetOutputs show
				infoHandler <- newSingleHandler (Just "infos") show
				manager <- newManager workers seeds [] ["text/html"] [] rejectedSuffixes httpHandler invalidHandler visitedHandler externalHandler rejectedHandler
					internetHandler infoHandler
				forever $ readLn >>= putStrLn
