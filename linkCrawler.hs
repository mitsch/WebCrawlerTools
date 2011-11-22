module Main where
	import Text.HTML.TagSoup
	import Data.Bool
	import Network.HTTP
	import Data.String.Utils
	import System.Console.GetOpt
	import Control.Concurrent

	while :: IO Bool -> IO ()
	while action = case action of { IO True -> while action; IO False -> return ()}
	-------------------------------------------------------------------
	proceedCrawlingLink :: MVar [String] -> String -> IO Bool
	proceedCrawlingLink visitedLinks link = modifyMVar visitedLinks (\v -> case any (link== v) of
		True -> return (v,False)
		False -> return ((link:v), True))
	--- ---------------------------------------------------------------
	getHTMLAttributes :: [String] -> String -> [String]
	getHTMLAttributes attributes document =
		let openTags = filter isTagOpen (parseTags document)
		in filter (not. null) (concat $ map (\tag -> map (\attribute -> fromAttrib atribute tag) attributes) openTags)

	filterAttributes :: [String] -> [String] -> [String]
	filterAttributes rejectSuffixes attributes = filter (\attribute -> not $ any (\suffix -> endswith suffix attribute) rejectSuffixes) attributes

	completeRelativeLinks :: String -> [String] -> [String]
	completeRelativeLinks originUrl links = map completeRelativeLink links
		where completeRelativeLink link = case startswith "/" link of
			True -> originUrl ++ link
			False -> link

	splitList :: (a -> Bool) -> [a] -> ([a],[a])
	splitList f l = aux f l [] [] where
		aux f (x:xs) matched unmatched = case f x of {True -> aux f xs (x:matched) unmatched; False -> aux f xs matched (x:unmatched)}
		aux f [] matched unmatched = (matched, unmatched)
	-- ----------------------------------------------------------------
	data ConsoleArgument = SeedsFile FilePath | RejectedSuffixesFile FilePath
	
	consoleArgumentDescriptions :: [OptDescr ConsoleArgument]
	consoleArgumentDescriptions = [Option [] ["seeds-file"] (ReqArg (\path -> SeedsFile path) "FILE") "file of links to start to crawl",
																 Option [] ["rejected-suffixes-file"] (path -> RejectedSuffixesFile path) "file of suffixes to reject"]

	sortArguments :: [ConsoleArgument] -> ([FilePath], [FilePath])
	sortArguments args = sortArgument' args [] [] where
		sortArgument' (arg:args) seeds suffixes = case arg of
			SeedsFile path -> sortArgument' args (path:seeds) suffixes
			RejectedSuffixesFile path -> sortArgument' args seeds (path:suffixes)
		sortArgument' [] seeds suffixes = (seeds, suffixes)

	data ConsoleArgumentCompilationResult = CompilationSuccess [String] [String] | CompilationFailure [String]
	
	compileConsoleArguments :: [String] -> IO ConsoleArgumentCompilationResult
	compileConsoleArguments arguments = case getOpt RequireOrder consoleArgumentDescriptions arguments of
		(args,_,[]) -> let (seedFiles, suffixFiles) = sortArguments args
			in return $ CompilationSuccess (concat $ map getFromFile seedFiles) (concat $ map getFomFile suffixFiles) where
				getFromFile path = readFile path >>= (\content -> return $ lines content)
		(_,_,errors) -> return $ CompilationFailure errors ++ [usageInfo "usage: linkCrawler [arguments]" consoleArgumentDescriptions]

	-- ---------------------------------------------------------------------------
	crawlWorker :: Chan [String] -> [String] -> IO ()
	crawlWorker seeds suffixes = forever $ do
		seed <- readChan seeds
		case proceed seeds seed of
			True -> do
				response <- simpleHTTP (getRequest seed)
				case findHeader HdrContentType response of
					Just hdrValue -> case or $ map ("text/xml"==) (split "," hdrValue) of
						True -> do
							document <- getResponseBody response
							let links = (completeRelativeLinks seed). (filter (not. startswith "#")). (filterAttributes suffixes). getHTMLAttributes ["href","src"] document
							let (internal, external) = splitList (startswith seed) links
							writeList2Chan seeds internal
						False -> DetectWrongContentType seed hdrValue
					Nothing -> DetectWrongContentType seed ""	
			False -> return ()

	-- ----------------------------------------------------------------------------
	main = getArgs >>= compileConsoleArguments >>= (\compilationResult -> case compilationResult of
		CompilationFailure message -> mapM_ putStrLn message
		CompilationSuccess preSeeds preRejectedSuffixes -> do
			seeds <- newChan
			writeList2Chan seeds preSeeds
			ids <- replicateM 10 (forkIO $ crawlWorker seeds preRejectedSuffixes)
			while $ readLine >>= (\input -> return $ input /= "exit")
			mapM_ killThread ids
			putStrLn ""
		)
