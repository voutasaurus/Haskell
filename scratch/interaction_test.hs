main = interact respondByLine

echo :: String -> String
echo xs = xs

respondByLine = unlines . map (echo) . lines  


respondPassword = unlines . map (\xs -> if isPassword xs then "access granted" else "access denied") . lines  
    where   isPassword xs = xs == "test"  

respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines  
    where   isPalindrome xs = xs == reverse xs  