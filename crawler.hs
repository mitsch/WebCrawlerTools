module Crawler where
	import Text.HTML.TagSoup
	import Control.Concurrent.Chan
	import Data.Bool
	import qualified Data.Set as Set
	import Data.List
	import Data.String.Utils
	import Network.HTTP
	import Network.Stream
	import Control.Concurrent
	import Control.Monad
	import System.IO
	import Domain
	import System.IO.Error

	data Manager = Manager {seedChannel :: Chan String,
													visitedPages :: MVar (Set.Set String),
													acceptedHeaders :: [String],
													acceptedDomains :: [Domain],
													rejectedSuffixes :: MVar [String],
													handleHTTPFailure :: Manager -> String -> IO (),
													handleContentTypeFailure :: Manager -> (String, String) -> IO (),
													handleVisiteds :: Manager -> [String] -> IO (),
													handleExternals :: Manager -> [String] -> IO (),
													handleRejecteds :: Manager -> [String] -> IO (),
													handleInternetFailure :: Manager -> String -> IO (),
													handleInformations :: Manager -> String -> IO ()
													}

	data HeaderAcceptance = AcceptHeader | RejectHeader String

	newManager :: Int -> [String] -> [String] -> [String] -> [Domain] -> [String] -> (Manager -> String -> IO ()) -> (Manager -> (String, String) -> IO ()) -> (Manager -> [String] -> IO ()) ->  (Manager -> [String] -> IO ()) ->  (Manager -> [String] -> IO ()) -> (Manager -> String -> IO ()) -> (Manager -> String -> IO ()) -> IO Manager
	newManager workers seeds visiteds headers domains suffixes httpFailure contentTypeFailure visits externals rejecteds internetFailures infos = do
		seedChan <- newChan
		writeList2Chan seedChan seeds
		visitedPagesVar <- newMVar (Set.fromList visiteds)
		rejectedSuffixesVar <- newMVar suffixes
		let manager = Manager seedChan visitedPagesVar headers (domains ++ map (removeWWW.fromURL) seeds) rejectedSuffixesVar httpFailure contentTypeFailure visits externals rejecteds internetFailures infos
		ids <- replicateM workers (forkIO $ forever $ (getNextSeed manager >>= crawlPage manager))
		(handleInformations manager) manager ("started " ++ show workers ++ " worker threads ...")
		return manager

	getNextSeed :: Manager -> IO String
	getNextSeed manager = do
		seed <- readChan $ seedChannel manager
		(handleInformations manager) manager ("parsing " ++ seed)
		visited <- isPageVisited manager seed
		case visited of
			True -> getNextSeed manager
			False -> return seed

	isPageVisited :: Manager -> String -> IO Bool
	isPageVisited manager url = withMVar (visitedPages manager) (\s -> return $ Set.member url s)

	crawlPage :: Manager -> String -> IO ()
	crawlPage manager seed = do
		connectionTry <- try $ simpleHTTP(getRequest seed)
		case connectionTry of
			Left exception ->
		result <- simpleHTTP (getRequest seed)
		case result of
			Right response -> do
				case hasCorrectHeader manager response of
					AcceptHeader -> do
						body <- getResponseBody result
						let links = filter (\l -> not $ (startswith "#" l || startswith "mailto:" l)) (getLinks ["href", "src"] body)
						let (internal, notInternal) = partition (\l -> not (startswith "http://" l || startswith "https://" l)) links
						(subExternal, notSubExternal) <- partitionExternalDomains manager notInternal
						(seeds, notSeeds) <- partitionRejectedSuffixes manager ((map (seed++) internal) ++ subExternal)
						writeList2Chan (seedChannel manager) seeds
						(handleVisiteds manager) manager seeds
						(handleExternals manager) manager notSubExternal
						(handleRejecteds manager) manager notSeeds
						(handleInformations manager) manager (seed ++ " produced " ++ show (length seeds) ++ " seeds")
					RejectHeader header -> do
						let pathSegments = drop 3 (split "/" seed)
						case null pathSegments of
							True -> return ()
							False -> do
								let lastSegment = last pathSegments
								case any ('.'==) lastSegment of
									False -> return ()
									True -> do
										let suffix = last $ split "." lastSegment
										modifyMVar_ (rejectedSuffixes manager) (\suffixes -> return (suffix:suffixes))				
						(handleContentTypeFailure manager) manager (seed, header)
			_ -> (handleHTTPFailure manager) manager seed

	hasCorrectHeader :: Manager -> Response String -> HeaderAcceptance
	hasCorrectHeader manager response = 
		case find (\header -> hdrName(header) == HdrContentType) (rspHeaders response) of
			Just (Header name value) -> case any (\t -> any (t==) (acceptedHeaders manager)) (split ";" value) of
				True -> AcceptHeader
				False -> RejectHeader value
			Nothing -> RejectHeader ""
	
	getLinks :: [String] -> String -> [String]
	getLinks attributes document =
		let openTags = filter isTagOpen (parseTags document)
		in filter (not. null) (concat $ map (\tag -> map (\attribute -> fromAttrib attribute tag) attributes) openTags)

	partitionExternalDomains :: Manager -> [String] -> IO ([String], [String])
	partitionExternalDomains manager links = return $ partition (\l -> any ((fromURL l)==) (acceptedDomains manager)) links
		
	partitionRejectedSuffixes :: Manager -> [String] -> IO ([String], [String])
	partitionRejectedSuffixes manager links = do
		suffixes <- readMVar $ rejectedSuffixes manager
		return $ partition (\link -> not $ any (\suffix -> endswith suffix link) suffixes) links
