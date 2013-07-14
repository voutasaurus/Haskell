import System.IO     

main = do   
	putStrLn "Please enter the name of the file you wish to append a line to:"
	filename <- getLine 
	if null filename  
        then return ()  
        else addLineTo filename
			
			
addLineTo filename = do  
			putStrLn "Please the line you wish to append: (leave blank to terminate)"
			line <- getLine  
			if null line
				then return ()
				else do
					appendFile (filename ++ ".txt") (line ++ "\n") 
					putStrLn ("The line: \n " ++ line ++ "\n  " ++ "added to " ++ filename ++ ".txt")
					addLineTo filename