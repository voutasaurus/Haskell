import System.IO     

main = do
	putStrLn "Choice: Which would you like? A or B"
	choice <- getLine
	if null choice
		then do
			putStrLn "You didn't enter anything. The program will close now."
			return ()
		else do
			putStrLn ( "You have selected: " ++ choice ++ ". Accessing data now." )
			choose choice
			putStrLn "Choice complete, restarting program."
			main
					
choose choice = do
	if choice == "A"
		then choice_A
		else choice_B

choice_A = do
	putStrLn "You chose A. I suppose that's not the worst decision anybody has ever made."
	putStrLn "Are you done being A? (y/n)"
	goback <- getLine
	if goback == "y"
		then return ()
		else choice_A

choice_B = do
	putStrLn "You chose B. I guess that's okay. At least you didn't projectile vomit on your computer."
	putStrLn "Are you done being B? (y/n)"
	goback <- getLine
	if goback == "y"
		then return ()
		else choice_B

