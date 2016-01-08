extractCommandArgs :: String -> LaTeX -> Maybe [TeXArg]



extractCommandArgs s (TeXSeq lt rt) = 
	if isJust lst 
		then lst 
		else rst
  where lst = extractCommandArgs s lt
        rst = extractCommandArgs s rt



extractCommandArgs s (TeXComm name args)
  | name == s = Just args
  | otherwise = Nothing



extractCommandArgs _ _ = Nothing