module CommandLine where

	import System.Console.GetOpt
	import Data.Either
	import Data.List
	import Data.Maybe

	data ConsoleArgument = SeedsFile FilePath |
												 RejectedSuffixesFile FilePath |
												 WorkerCount Int |
												 VisitedLinksOutput FilePath |
												 RejectedLinksOutput FilePath |
												 InvalidContentTypeOutput FilePath |
												 ExternalLinksOutput FilePath |
												 HTTPFailureOutput FilePath |
												 InternetFailureOutput FilePath

	getSeedFiles :: [ConsoleArgument] -> ([FilePath], [ConsoleArgument])
	getSeedFiles arguments = let (seedFiles, notSeedFiles) = partition (\a -> case a of {SeedsFile _ -> True; _ -> False}) arguments
		in (map (\f -> let SeedsFile path = f in path) seedFiles, notSeedFiles)

	getRejectedSuffixFiles :: [ConsoleArgument] -> ([FilePath], [ConsoleArgument])
	getRejectedSuffixFiles arguments = let (suffixFiles, notSuffixFiles) = partition (\a -> case a of {RejectedSuffixesFile _ -> True; _ -> False}) arguments
		in (map (\f -> let RejectedSuffixesFile path = f in path) suffixFiles, notSuffixFiles)

	getWorkerCounts :: [ConsoleArgument] -> ([Int], [ConsoleArgument])
	getWorkerCounts arguments = let (workerCounts, notWorkerCounts) = partition (\a -> case a of {WorkerCount _ -> True; _ -> False}) arguments
		in (map (\w -> let WorkerCount count = w in count) workerCounts, notWorkerCounts)

	getVisitedLinksOutputs :: [ConsoleArgument] -> ([FilePath], [ConsoleArgument])
	getVisitedLinksOutputs arguments = let (vo, notVO) = partition (\a -> case a of {VisitedLinksOutput _ -> True; _ -> False}) arguments
		in (map (\v -> let VisitedLinksOutput path = v in path) vo, notVO)

	getRejectedLinksOutputs :: [ConsoleArgument] -> ([FilePath], [ConsoleArgument])
	getRejectedLinksOutputs arguments = let (ro, notRO) = partition (\a -> case a of {RejectedLinksOutput _ -> True; _ -> False}) arguments
		in (map (\r -> let RejectedLinksOutput path = r in path) ro, notRO)

	getInvalidContentTypeOutputs :: [ConsoleArgument] -> ([FilePath], [ConsoleArgument])
	getInvalidContentTypeOutputs arguments = let (io, notIO) = partition (\a -> case a of {InvalidContentTypeOutput _ -> True; _ -> False}) arguments
		in (map (\i -> let InvalidContentTypeOutput path = i in path) io, notIO)

	getExternalLinksOutputs :: [ConsoleArgument] -> ([FilePath], [ConsoleArgument])
	getExternalLinksOutputs arguments = let (eo, notEO) = partition (\a -> case a of {ExternalLinksOutput _ -> True; _ -> False}) arguments
		in (map (\e -> let ExternalLinksOutput path = e in path) eo, notEO)

	getHTTPFailureOutputs :: [ConsoleArgument] -> ([FilePath], [ConsoleArgument])
	getHTTPFailureOutputs arguments = let (ho, notHO) = partition (\a -> case a of {HTTPFailureOutput _ -> True; _ -> False}) arguments
		in (map (\h -> let HTTPFailureOutput path = h in path) ho, notHO)

	getInternetFailureOutputs :: [ConsoleArgument] -> ([FilePath], [ConsoleArgument])
	getInternetFailureOutputs arguments = let (io, notIO) = partition (\a -> case a of {InternetFailureOutput _ -> True; _ -> False}) arguments
		in (map (\i -> let InternetFailureOutput path = i in path) io, notIO)

	consoleArgumentDescriptions :: [OptDescr ConsoleArgument]
	consoleArgumentDescriptions = [Option ['s'] ["seeds-file"] (ReqArg (\path -> SeedsFile path) "FILE") "file of links to start to crawl",
																 Option [] ["rejected-suffixes-file"] (ReqArg (\path -> RejectedSuffixesFile path) "FILE") "file of suffixes to reject",
																 Option ['w'] ["worker-count"] (ReqArg (\count -> WorkerCount (read count)) "COUNT") "count of worker threads",
																 Option ['v'] ["visited-output"] (ReqArg (\path -> VisitedLinksOutput path) "FILE") "path to print all visited links",
																 Option ['r'] ["rejected-output"] (ReqArg (\path -> RejectedLinksOutput path) "FILE") "path to print all rejected links",
																 Option ['i'] ["invalid-output"] (ReqArg (\path -> InvalidContentTypeOutput path) "FILE")
																 	"path to print all links with invalid content type",
																 Option ['e'] ["external-output"] (ReqArg (\path -> ExternalLinksOutput path) "FILE") "path to print all external links",
																 Option ['h'] ["http-failure-output"] (ReqArg (\path -> HTTPFailureOutput path) "FILE")
																 	"path to print all links which couldn't get reached due to http failure",
																 Option ['t'] ["internet-failure-output"] (ReqArg (\path -> InternetFailureOutput path) "FILE")
																 	"path to print all links which couldn't get reached due to internet failure"]


	data ConsoleArgumentCompilationResult = CompilationFailure [String] |
		CompilationSuccess [String] [String] Int (Maybe FilePath) (Maybe FilePath) (Maybe FilePath) (Maybe FilePath) (Maybe FilePath) (Maybe FilePath)

	compileConsoleArguments :: [String] -> IO ConsoleArgumentCompilationResult
	compileConsoleArguments arguments =	let
			getFromFile path = readFile path >>= (\content -> return $ lines content)
			formulateError errors = errors ++ [usageInfo "usage: linkCrawler [arguments]" consoleArgumentDescriptions]
		in case getOpt RequireOrder consoleArgumentDescriptions arguments of
			(args,_,[]) -> do
				let (workerCounts, notWorkerCounts) = getWorkerCounts args
				let (externalOutputs, notExternalOutputs) = getExternalLinksOutputs notWorkerCounts
				let (invalidOutputs, notInvalidOutputs) = getInvalidContentTypeOutputs notExternalOutputs
				let (rejectedOutputs, notRejectedOutputs) = getRejectedLinksOutputs notInvalidOutputs
				let (visitedOutputs, notVisitedOutputs) = getVisitedLinksOutputs notRejectedOutputs
				let (suffixFiles, notSuffixFiles) = getRejectedSuffixFiles notVisitedOutputs
				let (seedFiles, notSeedFiles) = getSeedFiles notSuffixFiles
				let (httpOutputs, notHTTPOutputs) = getHTTPFailureOutputs notSeedFiles
				let (internetOutputs, notInternetOutput) = getInternetFailureOutputs notHTTPOutputs
				case length workerCounts > 1 of
					True -> return $ CompilationFailure (formulateError ["too many workercount arguments"])
					False -> do
						case length externalOutputs > 1 of
							True -> return $ CompilationFailure (formulateError ["too many external file outputs"])
							False -> do
								case length invalidOutputs > 1 of
									True -> return $ CompilationFailure (formulateError ["too many invalid file outputs"])
									False -> do
										case length rejectedOutputs > 1 of
											True -> return $ CompilationFailure (formulateError ["too many reject file outputs"])
											False -> do
												case length httpOutputs > 1 of
													True -> return $ CompilationFailure (formulateError ["too many http failure outputs"])
													False -> do
														case length internetOutputs > 1 of
															True -> return $ CompilationFailure (formulateError ["too many internet failure outputs"])
															False -> do
																case length visitedOutputs > 1 of
																	True -> return $ CompilationFailure (formulateError ["too many visited file outputs"])
																	False -> do
																		seeds <- mapM getFromFile seedFiles
																		suffixes <- mapM getFromFile suffixFiles
																		return $ CompilationSuccess (concat seeds) (concat suffixes)
																							(case length workerCounts of {1 -> head $ workerCounts; 0 -> 1})
																							(case length externalOutputs of {1 -> Just (head externalOutputs); 0 -> Nothing})
																							(case length invalidOutputs of {1 -> Just (head invalidOutputs); 0 -> Nothing})
																							(case length rejectedOutputs of {1 -> Just (head rejectedOutputs); 0 -> Nothing})
																							(case length visitedOutputs of {1 -> Just (head visitedOutputs); 0 -> Nothing})
																							(case length httpOutputs of {1 -> Just (head httpOutputs); 0 -> Nothing})
																							(case length internetOutputs of {1 -> Just (head internetOutputs); 0 -> Nothing})
			(_,_,errors) -> return $ CompilationFailure (formulateError errors)
