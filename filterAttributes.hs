module Main where
	import Text.HTML.TagSoup
	import System( getArgs )
	import System.Console.GetOpt
	import Data.String.Utils
	import Data.Bool

	{--
	 - gets all attributes in attr from document
	 -
	 - getAttributes (html document) (attribute key) (attribute values)
	--}
	getAttributes :: [String] -> [String] -> [String]
	getAttributes document attrs = concat $ map (getAttributesFromOpenTag attrs) (filter isTagOpen (parseTags document)) where
		getAttributesFromOpenTag attrs tag = filter (not. null) (map (\attr -> fromAttrib attr tag) attrs)
	
	{--
	 - filters out all links which matches the prefixes or suffixes on the to reject list, but if at least one prefix or suffix matches
	 - a link the link won't be filtered out regardless of the to-reject lists
	 -
	 - filterLinks (prefixes to accept) (prefixes to reject) (suffixes to accept) (suffixes to reject) (links to filter) (links not filtered out)
	--}
	filterLinks :: [String] -> [String] -> [String] -> [String] -> [String] -> [String]
	filterLinks aps rps ass rss links = filter (\l -> and $ or [matchPrefix aps, not. matchPrefix rps] ++ or [matchSuffix ass, not. matchSuffix rss]) links 
		where	matchPrefix prefixes link = any $ map (\p -> startswith p link) prefixes
					matchSuffix suffixes link = any $ map (\s -> endswith s link) suffixes

	data Flag = AcceptPrefix String | RejectPrefix String | AcceptSuffix String | RejectSuffix String |
							AcceptPrefixFile FilePath | RejectPrefixFile FilePath | AcceptSuffixFile FilePath | RejectSuffixFile FilePath |
							Help

	type FixCollection = ([String], [String], [String], [String], [FilePath], [FilePath], [FilePath], [FilePath], [Flag])

	collectFixFlags :: [Flag] -> FixCollection
	collectFixFlags flags = aux flags [] [] [] [] [] [] [] [] [] where
		aux (flag:flags) ap rp as rs apf rpf asf rsf unknown = case flag of
			AcceptPrefix pre -> aux flags pre:ap rp as rs apf rpf asf rsf unknown
			RejectPrefix pre -> aux flags accPre pre:rejPre asf rsf apf rpf asf rsf unknown
			AcceptSuffix suf -> aux flags ap rp suf:as rs apf rpf asf rsf unknown
			RejectSuffix suf -> aux flags ap rp as suf:rs apf rpf asf rsf unknown
			AcceptPrefixFile file -> aux flags ap rp as rs file:apf rpf asf rsf unknown
			RejectPrefixFile file -> aux flags ap rp as rs apf file:rpf asf rsf unknown
			AcceptSuffixFile file -> aux flags ap rp as rs apf rpf file:asf rsf unknown
			RejectSuffixFile file -> aux flags ap rp as rs apf rpf asf file:rsf unknown
			_ -> aux flags ap rp asf rsf apf rpf asf rsf flag:unknown
		aux [] ap rp as rs apf rpf asf rsf unknown = (ap, rp, as, rsf, apf, rpf, asf, rsf, unknown)

	getAcceptPrefixes :: FixCollection -> [String]
	getAcceptPrefixes (ap,_,_,_,_,_,_,_,_) = ap

	getRejectPrefixes :: FixCollection -> [String]
	getRejectPrefixes (_,rp,_,_,_,_,_,_,_) = rp

	getAcceptSuffixes :::FixCollection -> [String]
	getAcceptSuffixes (_,_,as,_,_,_,_,_,_) = as

	getRejectSuffixes :: FixCollection -> [String]
	getRejectSuffixes (_,_,_,rs,_,_,_,_,_) = rs

	getAcceptPrefixFiles :: FixCollection -> [FilePath]
	getAcceptPrefixFiles (_,_,_,_,apf,_,_,_,_) = apf

	getRejectPrefixFiles :: FixCollection -> [FilePath]
	getRejectPrefixFiles (_,_,_,_,_,rpf,_,_,_) = rpf

	getAcceptSuffixFiles :: FixCollection -> [FilePath]
	getAcceptSuffixFiles (_,_,_,_,_,_,asf,_,_) = asf

	getRejectSuffixFiles :: FixCollection -> [FilePath]
	getRejectSuffixFiles (_,_,_,_,_,_,_,rsf,_) = rsf

	getUnknownFlags :: FixCollection -> [Flag]
	getUnknownFlags (_,_,_,_,_,_,_,_,unknown) = unknown

	data OptCompResult = ExecFilter { acceptPrefixes :: [String],
																	  rejectPrefixes :: [String],
																		acceptSuffixes :: [String],
																		rejectSuffixes :: [String],
																		acceptPrefixFiles :: [FilePath],
																		rejectPrefixFiles :: [FilePath],
																		acceptSuffixFiles :: [FilePath],
																		rejectSuffixFiles :: [FilePath] } |
												DisplayHelp |
												ErrorOcurred [String]

	options :: [OptDescr Flag]
	options = [Option [] ["accept-prefix", "ap"] (ReqArg (\str -> AcceptPrefix str) "PREFIX") "accept always links having this prefix",
						 Option [] ["reject-prefix", "rp"] (ReqArg (\str -> RejectPrefix str) "PREFIX") "reject links having this prefix",
						 Option [] ["accept-suffix", "as"] (ReqArg (\str -> AcceptSuffix str) "SUFFIX") "accept always links having this suffix",
						 Option [] ["reject-suffix", "rs"] (ReqArg (\str -> RejectSuffix str) "SUFFIX") "reject links having this suffix",
						 Option [] ["accept-prefix-file", "apf"] (ReqArgs (\file -> AcceptPrefixFile file) "FILE")
						 	"file contains list of prefixes (one per line) to accept",
             Option [] ["reject-prefix-file", "rpf"] (ReqArgs (\file -> RejectPrefixFile file) "FILE")
						 	"file contains list of prefixes (one per line) to reject",
						 Option [] ["accept-suffix-file", "asf"] (ReqArgs (\file -> AcceptSuffixFile file) "FILE")
						 	"file contains list of suffixes (one per line) to accept",
						 Option [] ["reject-suffix-file", "rsf"] (ReqArgs (\file -> RejectSuffixFile file) "FILE")
						 	"file contains list of suffixes (one per line) to reject",
						 Option ['h'] ["help"] (NoArg Help) "prints out description"]

	getFixesFromFile :: FilePath -> IO [String]
	getFixesFromFile filePath = readFile filePath >>= return. lines

	data OptionsCompileResult = Exec [Flag] | Error [String]

	compileOptions :: [String] -> OptionsCompileResult
	compileOptions arguments = case getOpt RequireOrder options arguments of
			(opts,_,[]) -> Exec opts
			(_,_,errors) -> Error errors




	main = do
		args <- getArgs
		contents <- getContents
		let fixes = collectFixFlags args in 
	
	case compileOptions arguments of
		Error errors = putStrLn. usageInfo "filterArguments [options ...]" arguments
		Exec flags = mapMap_ putStrLn (getAttributes () ())

	main = getArgs >>= (\arguments -> case compileOptions arguments of
		Error errors = putStrLn. usageInfo "filterArguments [options ...]" arguments
		Exec flags = let rp = rejectPrefixs
		mapM_ putStrLn (filterLinks (rejectPrefixs append ) () () () (getAttributes contents ["href", "src"])))

		mapM_ putStrLn (filterLinks acceptPrefixes rejectPrefixes acceptSuffixes rejectSuffixes (getAttributes contents ["href", "src"]))
