module Main where
	import Text.HTML.TagSoup
	import System( getArgs )
	import System.Console.GetOpt
	import Data.String.Utils
	import Data.Bool
	import IO
	import qualified System.Exit as SE

	data Flag = AcceptPrefix String | RejectPrefix String | AcceptSuffix String | RejectSuffix String |
							AcceptPrefixFile FilePath | RejectPrefixFile FilePath | AcceptSuffixFile FilePath | RejectSuffixFile FilePath |
							Help

	isHelpFlag :: Flag -> Bool
	isHelpFlag Help = True
	isHelpFlag _ = False

	type FixCollection = ([String], [String], [String], [String], [FilePath], [FilePath], [FilePath], [FilePath], [Flag])

	collectFixFlags :: [Flag] -> FixCollection
	collectFixFlags flags = aux flags [] [] [] [] [] [] [] [] [] where
		aux (flag:flags) ap rp as rs apf rpf asf rsf unknown = case flag of
			AcceptPrefix pre -> aux flags (pre:ap) rp as rs apf rpf asf rsf unknown
			RejectPrefix pre -> aux flags ap (pre:rp) asf rsf apf rpf asf rsf unknown
			AcceptSuffix suf -> aux flags ap rp (suf:as) rs apf rpf asf rsf unknown
			RejectSuffix suf -> aux flags ap rp as (suf:rs) apf rpf asf rsf unknown
			AcceptPrefixFile file -> aux flags ap rp as rs (file:apf) rpf asf rsf unknown
			RejectPrefixFile file -> aux flags ap rp as rs apf (file:rpf) asf rsf unknown
			AcceptSuffixFile file -> aux flags ap rp as rs apf rpf (file:asf) rsf unknown
			RejectSuffixFile file -> aux flags ap rp as rs apf rpf asf (file:rsf) unknown
			_ -> aux flags ap rp asf rsf apf rpf asf rsf (flag:unknown)
		aux [] ap rp as rs apf rpf asf rsf unknown = (ap, rp, as, rsf, apf, rpf, asf, rsf, unknown)

	getAcceptPrefixes :: FixCollection -> [String]
	getAcceptPrefixes (ap,_,_,_,_,_,_,_,_) = ap

	getRejectPrefixes :: FixCollection -> [String]
	getRejectPrefixes (_,rp,_,_,_,_,_,_,_) = rp

	getAcceptSuffixes :: FixCollection -> [String]
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

	options :: [OptDescr Flag]
	options = [Option [] ["accept-prefix", "ap"] (ReqArg (\str -> AcceptPrefix str) "PREFIX") "accept always links having this prefix",
						 Option [] ["reject-prefix", "rp"] (ReqArg (\str -> RejectPrefix str) "PREFIX") "reject links having this prefix",
						 Option [] ["accept-suffix", "as"] (ReqArg (\str -> AcceptSuffix str) "SUFFIX") "accept always links having this suffix",
						 Option [] ["reject-suffix", "rs"] (ReqArg (\str -> RejectSuffix str) "SUFFIX") "reject links having this suffix",
						 Option [] ["accept-prefix-file", "apf"] (ReqArg (\file -> AcceptPrefixFile file) "FILE")
						 	"file contains list of prefixes (one per line) to accept",
             Option [] ["reject-prefix-file", "rpf"] (ReqArg (\file -> RejectPrefixFile file) "FILE")
						 	"file contains list of prefixes (one per line) to reject",
						 Option [] ["accept-suffix-file", "asf"] (ReqArg (\file -> AcceptSuffixFile file) "FILE")
						 	"file contains list of suffixes (one per line) to accept",
						 Option [] ["reject-suffix-file", "rsf"] (ReqArg (\file -> RejectSuffixFile file) "FILE")
						 	"file contains list of suffixes (one per line) to reject",
						 Option ['h'] ["help"] (NoArg Help) "prints out description"]

	getFixesFromFile :: FilePath -> IO [String]
	getFixesFromFile filePath = readFile filePath >>= return. lines

	data OptionsCompileResult = Exec [Flag] | Error [String]

	compileOptions :: [String] -> OptionsCompileResult
	compileOptions arguments = case getOpt RequireOrder options arguments of
			(opts,_,[]) -> Exec opts
			(_,_,errors) -> Error errors

	isError :: OptionsCompileResult -> Bool
	isError (Error _) = True
	isError _ = False

	getErrorFromResult :: OptionsCompileResult -> [String]
	getErrorFromResult (Error errors) = errors
	getErrorFromResult _ = error "wrong argument"

	getFlags :: OptionsCompileResult -> [Flag]
	getFlags (Exec flags) = flags
	getFlags _ = error "wrong argument"

	main = do {
						; arguments <- getArgs
						; let compileResult = compileOptions arguments
						; if isError compileResult
								then do {
												; mapM_ putStrLn (getErrorFromResult compileResult ++ [usageInfo "usage: filterAttributes [options]" options])
												; SE.exitWith $ SE.ExitFailure 1
												}
								else return ()
						; let fixes = collectFixFlags $ getFlags compileResult
						; if any isHelpFlag (getUnknownFlags fixes)
								then do {
												; putStrLn (usageInfo "usage: filterAttributes [options]" options)
												; SE.exitWith $ SE.ExitFailure 0
												}
								else return ()
						; acceptPrefixes <- mapM getFixesFromFile (getAcceptPrefixFiles fixes)
						; rejectPrefixes <- mapM getFixesFromFile (getRejectPrefixFiles fixes)
						; acceptSuffixes <- mapM getFixesFromFile (getAcceptSuffixFiles fixes)
						; rejectSuffixes <- mapM getFixesFromFile (getRejectSuffixFiles fixes)
						; contents <- getContents
						; let attributes = executeFilter (concat acceptPrefixes ++ getAcceptPrefixes fixes) (concat rejectPrefixes ++ getRejectPrefixes fixes) (concat acceptSuffixes ++ getAcceptSuffixes fixes) (concat rejectSuffixes ++ getRejectSuffixes fixes) ["href", "src"] contents
						; mapM_ putStrLn attributes
						}
	
	executeFilter :: [String] -> [String] -> [String] -> [String] -> [String] -> String -> [String]
	executeFilter acceptPrefixes rejectPrefixes acceptSuffixes rejectSuffixes attrFilters document = let
		attrs = filter (not. null) (concat $ map (\tag -> map (\filter -> fromAttrib filter tag) attrFilters) (filter isTagOpen (parseTags document)))
		in filterAttributes acceptPrefixes rejectPrefixes acceptSuffixes rejectSuffixes attrs

	filterAttributes :: [String] -> [String] -> [String] -> [String] -> [String] -> [String]
	filterAttributes acceptPrefixes rejectPrefixes acceptSuffixes rejectSuffixes attributes = let
		matchPrefix prefixes attribute = or $ map (\prefix -> startswith prefix attribute) prefixes
		matchSuffix suffixes attribute = or $ map (\suffix -> endswith suffix attribute) suffixes
		in filter (\attribute -> and [or [matchPrefix acceptPrefixes attribute, not $ matchPrefix rejectPrefixes attribute], or [matchSuffix acceptSuffixes attribute, not $ matchSuffix rejectSuffixes attribute]]) attributes
