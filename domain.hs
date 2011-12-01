module Domain where
	import Data.List.Utils
	
	data Domain = Domain [String]
	instance (Eq Domain) where
		Domain a == Domain b = all (\tuple -> let (first, second) = tuple in first == second) (zip a b)

	newDomain :: String -> Domain
	newDomain text = Domain (reverse $ split "." text)

	fromURL :: String -> Domain
	fromURL url = newDomain (takeWhile ('/'/=) (drop 2 (dropWhile ('/'/=) url)))

	removeWWW :: Domain -> Domain
	removeWWW (Domain a) = case last a of
		"www" -> Domain (init a)
		_ -> Domain a 
