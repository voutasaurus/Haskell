import System.IO     

main = do
	putStrLn "Welcome. Please enter your name:"
	name <- getLine
	if null name
		then do
			putStrLn "We have no names, man. No names. We are nameless!"
			return ()
		else do
			putStrLn ( "Hello, " ++ name ++ ". Please choose your password." )
			password <- getLine
			intermission
			entry <- checkPass password
			if entry
				then granted
				else denied
			return ()
					
denied =  do
	putStrLn "Access Denied"
	putStrLn "fin"

granted = do
	putStrLn "Access Granted"
	secret
	putStrLn "fin"

intermission = do
	putStrLn "This is the intermission."
	
checkPass password = do
	putStrLn "Enter your password please:"
	passEntry <- getLine
	return ( password == passEntry )

secret = do
	putStrLn "accessing secret files..."
		
echo :: String -> String
echo xs = xs

respondByLine = unlines . map (echo) . lines  


respondPassword = unlines . map (\xs -> if isPassword xs then "access granted" else "access denied") . lines  
    where   isPassword xs = xs == "test"  

respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines  
    where   isPalindrome xs = xs == reverse xs  