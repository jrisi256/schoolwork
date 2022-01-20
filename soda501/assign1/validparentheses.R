# R - Valid Parentheses
#
# Write a function that is passed a string s as an argument.
#
# Given a string s containing just the characters '(', ')', '{', '}', '[' and ']', determine if the input string is valid.
#
# An input string is valid if:
#
# Open brackets are closed by the same type of brackets.
# Open brackets are closed in the correct order.
#
# You may assume s is at least one character long (and less than say 10000 characters long) and consists of brackets only, i.e., the characters in '()[]{}'.

isValid <- function(s) {
    
    if(!is.character(s))
        return("Invalid variable type")
    
    string <- strsplit(s, "")[[1]]
    if(length(string) %in% c(0,1))
        return(FALSE)
    
    stack <- c()
    
    for(character in string) {
        
        if(character %in% c("{", "(", "["))
            stack <- append(stack, character)
        
        else if(character == "}") {
            if(stack[length(stack)] == "{")
                stack <- stack[-length(stack)]
            else
                return(FALSE)
        }
        
        else if(character == ")") {
            if(stack[length(stack)] == "(")
                stack <- stack[-length(stack)]
            else
                return(FALSE)
        }
        
        else if(character == "]") {
            if(stack[length(stack)] == "[")
                stack <- stack[-length(stack)]
            else
                return(FALSE)
        }
        
        else
            return("Invalid String")
    }
    
    if(length(stack) == 0)
        return(TRUE)
    else
        return(FALSE)
}

s <- "()"
test1 <- isValid(s) # TRUE

s <- "()[]{}"
test2 <- isValid(s) # TRUE

s <- "(]"
test3 <- isValid(s) # FALSE

s <- "({[({})]})"
test4 <- isValid(s) # TRUE

s <- "({[({})]})([{}]){}"
test5 <- isValid(s) # TRUE

s <- "([{})]{}"
test6 <- isValid(s) # FALSE

s <- "({[({})]})[(])"
test7 <- isValid(s) # FALSE

s <- "([((((()))))])"
test8 <- isValid(s) # TRUE

s <- "([((((())))])"
test9 <- isValid(s) # FALSE

all_test <- 
    all(test1, test2, !test3, test4, test5, !test6, !test7, test8, !test9)
