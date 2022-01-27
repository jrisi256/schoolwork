# R - Valid Palindrome
#
# Write a function that is passed a string s as an argument and checks whether the string is a palindrome or could be made a palindrome by the elimination of exactly one character.
#
# A palindrome reads the same backwards as forwards.
#
#
# You may assume s is at least one character long (and less than say 500 characters long) and consists of lowercase alphabetic characters only.

# return True if s is valid (a palindrome, or can be made one with elimination of 1 character)
# return False if s is invalid

isValidWithDeletion <- function(s){
    
    string <- strsplit(s, "")[[1]]
    palindrome <- T
    
    # Is the string a palindrome?
    for(i in seq(1:floor(length(string) / 2))) {
        if(string[i] != string[length(string) - i + 1]) {
            palindrome <- F
            break()
        }
    }
    
    # If it isn't a palindrome...
    if(!palindrome) {
        
        # Remove characters one-by-one and check if it's a palindrome
        for(i in seq(1:length(string))) {
            new_string <- string[-i]
            palindrome <- T
            
            for(i in seq(1:floor(length(new_string) / 2))) {
                if(new_string[i] != new_string[length(new_string) - i + 1]) {
                    palindrome <- F
                    break()
                }
            }
            
            # This means removing a letter turned the word into a palindrome
            if(palindrome)
                return(T)
        }
    }
    
    return(palindrome)
}

s <- "aba"
test1 <- isValidWithDeletion(s) # True (already a palindrome)

s <- "abca"
test2 <- isValidWithDeletion(s) # True (you could delete 'c')

s <- "abc"
test3 <- isValidWithDeletion(s) # False

s <- "abbcbcccbba"
test4 <- isValidWithDeletion(s) # True (you could delete the third 'b')

s <- "abccab"
test5 <- isValidWithDeletion(s) # False

s <- "abbcbczbzbba"
test6 <- isValidWithDeletion(s) # False

s <- "madamiamadam"
test7 <- isValidWithDeletion(s) # True (you could delete the 3rd 'a')

s <- "amanaplanacanalpanama"
test8 <- isValidWithDeletion(s) # True (already a palindrome)

s <- "manplancanalpanama"
test9 <- isValidWithDeletion(s) # False

works <- all(test1, test2, !test3, test4, !test5, !test6, test7, test8, !test9)
