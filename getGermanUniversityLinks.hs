module Main where
	import Text.HTML.TagSoup
	import Data.Maybe
	import Network.HTTP
	import System

	openURL url = getResponseBody =<< simpleHTTP (getRequest url)

	if' :: Bool -> a -> a -> a
	if' True first _ = first
	if' False _ second = second

	getAttribute :: Eq str => str -> [Attribute str] -> Maybe str
	getAttribute key (x:xs) =let (key1,value1) = x in if key == key1 then Just value1 else getAttribute key xs
	getAttribute key [] = Nothing
	
	isNewLineTagText :: (Tag String) -> Bool
	isNewLineTagText (TagText ('\n':app)) = all ('\t' ==) app
	isNewLineTagText _ = False

	getLinkFromSpecificWikiPage :: String -> String -> String
	getLinkFromSpecificWikiPage url document = fromMaybe url (listToMaybe $ catMaybes $ map extractLink (partitions (~== "<tr>") ((concat $ partitions (~== "<table id=\"Vorlage_Infobox_Hochschule\">") (filter (not. isNewLineTagText) (parseTags document)) )::[Tag String]))) where
		extractLink (TagOpen "tr" _ : TagOpen "th" _ : TagText "Website" : TagClose "th" : TagOpen "td" _ : TagOpen "a" attr : _) = getAttribute "href" attr
		extractLink _ = Nothing

	main = do
		args <- getArgs
		document <- openURL (if' (null args) "http://de.wikipedia.org/wiki/Liste_deutscher_Universit%C3%A4ten" (head args))
		let tags = (filter (not. isNewLineTagText) (parseTags document))
		let wikiLinks = map ("http://de.wikipedia.org" ++) (catMaybes $ map getLink (tail (partitions (~== "<tr>") (concat $ partitions (~== "<table class=\"wikitable sortable\">") tags)))) where
			getLink (TagOpen "tr" _ : TagOpen "td" _ : TagOpen "a" attr  : _) = getAttribute "href" attr
			getLink _ = Nothing
		links <- mapM (\url -> openURL url >>= (\document -> return $ getLinkFromSpecificWikiPage url document)) wikiLinks
		mapM_ putStrLn links
